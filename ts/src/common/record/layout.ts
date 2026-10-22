/**
 * Declarative COBOL record layouts.
 *
 * A layout is declared once per copybook with the builders below; it knows every
 * field's offset, length, PICTURE and USAGE, and can parse a fixed-width record
 * into a typed object and serialize the object back, byte-for-byte.
 *
 * ```ts
 * const ACCOUNT = defineRecord('ACCOUNT-RECORD', {
 *   acctId: num('ACCT-ID', '9(11)'),
 *   acctCurrBal: num('ACCT-CURR-BAL', 'S9(10)V99'),
 *   filler: alnum('FILLER', 'X(178)'),
 * });
 * const rec = ACCOUNT.parse(line);           // { acctId: Decimal, ... }
 * const bytes = ACCOUNT.serialize(rec);      // Buffer, same bytes as `line`
 * ```
 *
 * Numeric fields (DISPLAY zoned, COMP-3, COMP) are surfaced as decimal.js
 * `Decimal`s; alphanumeric and numeric-edited fields as strings.
 * REDEFINES items are optional views: they are only decoded when requested via
 * `parse(..., { views: true })`, and when present in the input object they are
 * written over the storage of the item they redefine (last writer wins, as with
 * COBOL storage).
 */
import { type Charset, decodeText, encodeText, padAlphanumeric } from '../codecs/charset.js';
import { Decimal, type NumericInput } from '../codecs/decimal.js';
import { decodeBinary, encodeBinary } from '../codecs/binary.js';
import { formatEdited } from '../codecs/edited.js';
import { decodePacked, encodePacked } from '../codecs/packed.js';
import { parsePic, type Pic, storageLength, type Usage } from '../codecs/pic.js';
import { decodeZoned, encodeZoned } from '../codecs/zoned.js';

/** VALUE clause: a literal, or `{ all: c }` for `VALUE ALL 'c'` / figurative constants. */
export type ValueClause = string | number | { all: string };

export interface ElementarySpec<C extends 'alphanumeric' | 'numeric' | 'numeric-edited' = 'alphanumeric' | 'numeric' | 'numeric-edited'> {
  readonly kind: 'elementary';
  readonly category: C;
  readonly cobolName: string;
  readonly pic: Pic;
  readonly usage: Usage;
  readonly value?: ValueClause;
  readonly length: number;
}

export interface GroupSpec<F extends Fields = Fields> {
  readonly kind: 'group';
  readonly cobolName: string;
  readonly fields: F;
}

export interface OccursSpec<I extends FieldSpec = FieldSpec> {
  readonly kind: 'occurs';
  readonly times: number;
  readonly item: I;
}

export interface RedefinesSpec<I extends FieldSpec = FieldSpec> {
  readonly kind: 'redefines';
  /** Key (in the same group) of the item being redefined. */
  readonly target: string;
  readonly item: I;
}

export type FieldSpec = ElementarySpec | GroupSpec | OccursSpec | RedefinesSpec;
export type Fields = { readonly [key: string]: FieldSpec };

// ---------------------------------------------------------------------------
// Type-level mapping spec -> parsed object / serialize input
// ---------------------------------------------------------------------------

export type ValueOf<S> =
  S extends ElementarySpec<infer C>
    ? C extends 'numeric'
      ? Decimal
      : string
    : S extends GroupSpec<infer F>
      ? RecordOf<F>
      : S extends OccursSpec<infer I>
        ? ValueOf<I>[]
        : S extends RedefinesSpec<infer I>
          ? ValueOf<I>
          : never;

type RedefKeys<F> = { [K in keyof F]: F[K] extends RedefinesSpec ? K : never }[keyof F];
type BaseKeys<F> = Exclude<keyof F, RedefKeys<F>>;

export type RecordOf<F> = { [K in BaseKeys<F>]: ValueOf<F[K]> } & { [K in RedefKeys<F>]?: ValueOf<F[K]> };

export type InputOf<S> =
  S extends ElementarySpec<infer C>
    ? C extends 'numeric'
      ? NumericInput
      : C extends 'numeric-edited'
        ? string | NumericInput
        : string
    : S extends GroupSpec<infer F>
      ? RecordInput<F>
      : S extends OccursSpec<infer I>
        ? (InputOf<I> | undefined)[]
        : S extends RedefinesSpec<infer I>
          ? InputOf<I>
          : never;

/** Serialize input: every field optional (missing fields get their VALUE / INITIALIZE default). */
export type RecordInput<F> = { [K in keyof F]?: InputOf<F[K]> };

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

/** Alphanumeric item, `PIC X(n)`. */
export function alnum(cobolName: string, picture: string, value?: ValueClause): ElementarySpec<'alphanumeric'> {
  const pic = parsePic(picture);
  if (pic.category !== 'alphanumeric') throw new Error(`${cobolName}: PIC ${picture} is not alphanumeric`);
  return { kind: 'elementary', category: 'alphanumeric', cobolName, pic, usage: 'DISPLAY', value, length: pic.displayLength };
}

/** Numeric item, `PIC S9(n)V9(m)` with optional USAGE (default DISPLAY = zoned decimal). */
export function num(
  cobolName: string,
  picture: string,
  opts: { usage?: Usage; value?: ValueClause } = {},
): ElementarySpec<'numeric'> {
  const pic = parsePic(picture);
  if (pic.category !== 'numeric') throw new Error(`${cobolName}: PIC ${picture} is not numeric`);
  const usage = opts.usage ?? 'DISPLAY';
  return { kind: 'elementary', category: 'numeric', cobolName, pic, usage, value: opts.value, length: storageLength(pic, usage) };
}

/** Numeric-edited item, e.g. `PIC -ZZZ,ZZZ,ZZZ.ZZ`. */
export function edited(cobolName: string, picture: string, value?: ValueClause): ElementarySpec<'numeric-edited'> {
  const pic = parsePic(picture);
  if (pic.category !== 'numeric-edited') throw new Error(`${cobolName}: PIC ${picture} is not numeric-edited`);
  return { kind: 'elementary', category: 'numeric-edited', cobolName, pic, usage: 'DISPLAY', value, length: pic.displayLength };
}

export function group<F extends Fields>(cobolName: string, fields: F): GroupSpec<F> {
  return { kind: 'group', cobolName, fields };
}

export function occurs<I extends FieldSpec>(times: number, item: I): OccursSpec<I> {
  return { kind: 'occurs', times, item };
}

export function redefines<I extends FieldSpec>(target: string, item: I): RedefinesSpec<I> {
  return { kind: 'redefines', target, item };
}

// ---------------------------------------------------------------------------
// Layout computation
// ---------------------------------------------------------------------------

export function specLength(spec: FieldSpec): number {
  switch (spec.kind) {
    case 'elementary':
      return spec.length;
    case 'group':
      return Object.values(spec.fields).reduce((n, f) => n + (f.kind === 'redefines' ? 0 : specLength(f)), 0);
    case 'occurs':
      return spec.times * specLength(spec.item);
    case 'redefines':
      return specLength(spec.item);
  }
}

/** Flattened metadata of one elementary item. */
export interface FieldMeta {
  /** Dotted JS path, e.g. `tranCatKey.trancatAcctId` or `expCustAddrLines[1].expCustAddrLine`. */
  readonly path: string;
  readonly cobolName: string;
  readonly offset: number;
  readonly length: number;
  readonly category: ElementarySpec['category'];
  readonly pic: Pic;
  readonly usage: Usage;
  /** True when this item is (inside) a REDEFINES view. */
  readonly inRedefines: boolean;
}

/** Metadata of any item (elementary or group), used for key definitions. */
export interface ItemMeta {
  readonly path: string;
  readonly cobolName: string;
  readonly offset: number;
  readonly length: number;
  readonly spec: FieldSpec;
}

export interface ParseOptions {
  charset?: Charset;
  /** Decode REDEFINES views: `true` for all, or the keys/paths of the views wanted. */
  views?: boolean | readonly string[];
}

export interface SerializeOptions {
  charset?: Charset;
}

export class RecordLayout<F extends Fields = Fields> {
  readonly length: number;
  readonly root: GroupSpec<F>;
  /** All elementary items, in storage order, OCCURS expanded. */
  readonly fields: readonly FieldMeta[];
  /** All items (groups included), keyed by path and by COBOL name (first occurrence wins). */
  private readonly items = new Map<string, ItemMeta>();

  constructor(
    readonly name: string,
    fields: F,
  ) {
    this.root = group(name, fields);
    this.length = specLength(this.root);
    const flat: FieldMeta[] = [];
    this.collect(this.root, '', 0, false, flat, true);
    this.fields = flat;
  }

  private collect(spec: FieldSpec, path: string, offset: number, inRedef: boolean, out: FieldMeta[], isRoot = false): void {
    const length = specLength(spec);
    if (!isRoot) {
      const cobolName = spec.kind === 'elementary' || spec.kind === 'group' ? spec.cobolName : '';
      const meta: ItemMeta = { path, cobolName, offset, length, spec };
      this.items.set(path, meta);
      if (cobolName && cobolName !== 'FILLER' && !this.items.has(cobolName)) this.items.set(cobolName, meta);
    }
    switch (spec.kind) {
      case 'elementary':
        out.push({ path, cobolName: spec.cobolName, offset, length, category: spec.category, pic: spec.pic, usage: spec.usage, inRedefines: inRedef });
        return;
      case 'group': {
        let cursor = offset;
        const offsets = new Map<string, number>();
        for (const [key, child] of Object.entries(spec.fields)) {
          const childPath = path ? `${path}.${key}` : key;
          if (child.kind === 'redefines') {
            const targetOffset = offsets.get(child.target);
            if (targetOffset === undefined) throw new Error(`${this.name}: REDEFINES target '${child.target}' not found before '${key}'`);
            this.collect(child.item, childPath, targetOffset, true, out);
            const meta = this.items.get(childPath)!;
            this.items.set(childPath, { ...meta, spec: child });
          } else {
            offsets.set(key, cursor);
            this.collect(child, childPath, cursor, inRedef, out);
            cursor += specLength(child);
          }
        }
        return;
      }
      case 'occurs': {
        const itemLen = specLength(spec.item);
        for (let i = 0; i < spec.times; i++) this.collect(spec.item, `${path}[${i}]`, offset + i * itemLen, inRedef, out);
        return;
      }
      case 'redefines':
        this.collect(spec.item, path, offset, true, out);
        return;
    }
  }

  /** Looks up an item (elementary or group) by JS path or COBOL name. */
  item(pathOrCobolName: string): ItemMeta {
    const meta = this.items.get(pathOrCobolName);
    if (!meta) throw new Error(`${this.name}: no item '${pathOrCobolName}'`);
    return meta;
  }

  /** Byte range of an item, e.g. for VSAM key definitions. */
  range(pathOrCobolName: string): { offset: number; length: number } {
    const { offset, length } = this.item(pathOrCobolName);
    return { offset, length };
  }

  /** A standalone layout for a group / REDEFINES sub-item (offsets relative to that item). */
  sub(key: string): RecordLayout {
    const spec = this.root.fields[key];
    const inner = spec?.kind === 'redefines' ? spec.item : spec;
    if (!inner || inner.kind !== 'group') throw new Error(`${this.name}: '${key}' is not a group`);
    return new RecordLayout(inner.cobolName, inner.fields);
  }

  /** Parses a fixed-width record. Short input is padded with spaces; long input is rejected. */
  parse(input: Uint8Array | string, opts: ParseOptions = {}): RecordOf<F> {
    const charset = opts.charset ?? 'ascii';
    const buf = toBuffer(input, this.length, charset, this.name);
    return parseGroup(this.root, buf, 0, charset, opts.views ?? false, '') as RecordOf<F>;
  }

  /** Serializes an object into a fixed-width record (Buffer). */
  serialize(record: RecordInput<F>, opts: SerializeOptions = {}): Buffer {
    const charset = opts.charset ?? 'ascii';
    const buf = Buffer.alloc(this.length);
    writeSpec(this.root, record, buf, 0, charset);
    return buf;
  }

  /** Serializes to a Latin-1 string (convenient for ASCII line-sequential files). */
  serializeText(record: RecordInput<F>): string {
    return this.serialize(record, { charset: 'ascii' }).toString('latin1');
  }

  /** A record initialised from VALUE clauses (INITIALIZE semantics for items without VALUE). */
  initial(): RecordOf<F> {
    return this.parse(this.serialize({}));
  }

  /** COBOL `IS NUMERIC` test of one numeric item in a raw record. */
  isNumeric(input: Uint8Array | string, pathOrCobolName: string, charset: Charset = 'ascii'): boolean {
    const buf = toBuffer(input, this.length, charset, this.name);
    const meta = this.item(pathOrCobolName);
    const spec = meta.spec.kind === 'redefines' ? meta.spec.item : meta.spec;
    if (spec.kind !== 'elementary') throw new Error(`${pathOrCobolName} is not elementary`);
    const bytes = buf.subarray(meta.offset, meta.offset + meta.length);
    if (spec.category !== 'numeric') return /^[0-9]*$/.test(decodeText(bytes, charset));
    return decodeElementaryNumeric(spec, bytes, charset).valid;
  }
}

/** Non-generic view of any layout (for tables of layouts, generic tooling). */
export interface AnyRecordLayout {
  readonly name: string;
  readonly length: number;
  readonly fields: readonly FieldMeta[];
  parse(input: Uint8Array | string, opts?: ParseOptions): Record<string, unknown>;
  serialize(record: Record<string, unknown>, opts?: SerializeOptions): Buffer;
  serializeText(record: Record<string, unknown>): string;
  item(pathOrCobolName: string): ItemMeta;
  range(pathOrCobolName: string): { offset: number; length: number };
}

export function defineRecord<F extends Fields>(name: string, fields: F): RecordLayout<F> {
  return new RecordLayout(name, fields);
}

// ---------------------------------------------------------------------------
// Parse / serialize implementation
// ---------------------------------------------------------------------------

function toBuffer(input: Uint8Array | string, length: number, charset: Charset, name: string): Buffer {
  let buf = typeof input === 'string' ? Buffer.from(input, 'latin1') : Buffer.from(input.buffer, input.byteOffset, input.byteLength);
  if (buf.length > length) throw new Error(`${name}: record is ${buf.length} bytes, layout is ${length}`);
  if (buf.length < length) {
    const padded = Buffer.alloc(length, charset === 'ebcdic' ? 0x40 : 0x20);
    buf.copy(padded);
    buf = padded;
  }
  return buf;
}

function decodeElementaryNumeric(spec: ElementarySpec, bytes: Uint8Array, charset: Charset) {
  switch (spec.usage) {
    case 'DISPLAY':
      return decodeZoned(bytes, spec.pic, charset);
    case 'COMP-3':
    case 'PACKED-DECIMAL':
      return decodePacked(bytes, spec.pic);
    default:
      return decodeBinary(bytes, spec.pic);
  }
}

/** Decodes one elementary item from a buffer. */
export function readElementary(spec: ElementarySpec, buf: Uint8Array, offset: number, charset: Charset): string | Decimal {
  const bytes = buf.subarray(offset, offset + spec.length);
  if (spec.category === 'numeric') return decodeElementaryNumeric(spec, bytes, charset).value;
  return decodeText(bytes, charset);
}

/** Encodes one elementary item into a buffer (COBOL MOVE semantics). */
export function writeElementary(
  spec: ElementarySpec,
  value: string | NumericInput | undefined,
  buf: Buffer,
  offset: number,
  charset: Charset,
): void {
  let bytes: Buffer;
  if (value === undefined) {
    bytes = defaultBytes(spec, charset);
  } else if (spec.category === 'alphanumeric') {
    if (typeof value !== 'string') throw new TypeError(`${spec.cobolName}: expected string`);
    bytes = encodeText(padAlphanumeric(value, spec.length), charset);
  } else if (spec.category === 'numeric-edited') {
    // Plain numeric strings / numbers are edited per the PICTURE; other strings are already-edited text.
    const text = typeof value === 'string' && !/^[+-]?\d+(\.\d+)?$/.test(value) ? value : formatEdited(spec.pic.source, value);
    bytes = encodeText(padAlphanumeric(text, spec.length), charset);
  } else {
    if (typeof value === 'string' && value.trim() === '') value = '0';
    switch (spec.usage) {
      case 'DISPLAY':
        bytes = encodeZoned(value, spec.pic, charset);
        break;
      case 'COMP-3':
      case 'PACKED-DECIMAL':
        bytes = encodePacked(value, spec.pic);
        break;
      default:
        bytes = encodeBinary(value, spec.pic);
    }
  }
  bytes.copy(buf, offset);
}

function defaultBytes(spec: ElementarySpec, charset: Charset): Buffer {
  const v = spec.value;
  if (v !== undefined && typeof v === 'object') {
    return encodeText(v.all.repeat(Math.ceil(spec.length / Math.max(1, v.all.length))).slice(0, spec.length), charset);
  }
  if (spec.category === 'alphanumeric') return encodeText(padAlphanumeric(v === undefined ? '' : String(v), spec.length), charset);
  if (spec.category === 'numeric-edited') {
    return encodeText(padAlphanumeric(typeof v === 'string' ? v : formatEdited(spec.pic.source, v ?? 0), spec.length), charset);
  }
  const out = Buffer.alloc(spec.length);
  writeElementary(spec, v ?? 0, out, 0, charset);
  return out;
}

function wantView(views: boolean | readonly string[], key: string, path: string): boolean {
  if (typeof views === 'boolean') return views;
  return views.includes(key) || views.includes(path);
}

function parseSpec(spec: FieldSpec, buf: Buffer, offset: number, charset: Charset, views: boolean | readonly string[], path: string): unknown {
  switch (spec.kind) {
    case 'elementary':
      return readElementary(spec, buf, offset, charset);
    case 'group':
      return parseGroup(spec, buf, offset, charset, views, path);
    case 'occurs': {
      const len = specLength(spec.item);
      return Array.from({ length: spec.times }, (_, i) => parseSpec(spec.item, buf, offset + i * len, charset, views, `${path}[${i}]`));
    }
    case 'redefines':
      return parseSpec(spec.item, buf, offset, charset, views, path);
  }
}

function parseGroup(spec: GroupSpec, buf: Buffer, offset: number, charset: Charset, views: boolean | readonly string[], path: string): Record<string, unknown> {
  const out: Record<string, unknown> = {};
  const offsets = new Map<string, number>();
  let cursor = offset;
  for (const [key, child] of Object.entries(spec.fields)) {
    const childPath = path ? `${path}.${key}` : key;
    if (child.kind === 'redefines') {
      if (wantView(views, key, childPath)) out[key] = parseSpec(child.item, buf, offsets.get(child.target)!, charset, views, childPath);
      continue;
    }
    offsets.set(key, cursor);
    out[key] = parseSpec(child, buf, cursor, charset, views, childPath);
    cursor += specLength(child);
  }
  return out;
}

function writeSpec(spec: FieldSpec, value: unknown, buf: Buffer, offset: number, charset: Charset): void {
  switch (spec.kind) {
    case 'elementary':
      writeElementary(spec, value as string | NumericInput | undefined, buf, offset, charset);
      return;
    case 'group': {
      const obj = (value ?? {}) as Record<string, unknown>;
      const offsets = new Map<string, number>();
      let cursor = offset;
      for (const [key, child] of Object.entries(spec.fields)) {
        if (child.kind === 'redefines') {
          if (obj[key] !== undefined) writeSpec(child.item, obj[key], buf, offsets.get(child.target)!, charset);
          continue;
        }
        offsets.set(key, cursor);
        writeSpec(child, obj[key], buf, cursor, charset);
        cursor += specLength(child);
      }
      return;
    }
    case 'occurs': {
      const arr = (value ?? []) as unknown[];
      const len = specLength(spec.item);
      for (let i = 0; i < spec.times; i++) writeSpec(spec.item, arr[i], buf, offset + i * len, charset);
      return;
    }
    case 'redefines':
      writeSpec(spec.item, value, buf, offset, charset);
      return;
  }
}

export { Decimal };
