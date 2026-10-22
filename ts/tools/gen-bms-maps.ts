/**
 * Generates src/online/framework/bms/maps.generated.ts from the BMS macros in
 * app/bms/*.bms (positions, ATTRB, COLOR, HILIGHT, INITIAL, PICOUT, JUSTIFY)
 * and cross-checks every named field against the symbolic maps in
 * app/cpy-bms/*.CPY (names, order, lengths). Run from ts/: `npm run gen:bms`.
 */
import { readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export interface GenField {
  name?: string;
  row: number;
  col: number;
  length: number;
  attrb: string[];
  color?: string;
  hilight?: string;
  initial?: string;
  picout?: string;
  picin?: string;
  justify?: string[];
}
export interface GenMap {
  mapset: string;
  map: string;
  size: [number, number];
  ctrl: string[];
  extatt: boolean;
  fields: GenField[];
}

/** Joins BMS statements: col 72 non-blank = continuation, next line resumes at col 16. */
function statements(src: string): { label: string; op: string; operands: string }[] {
  const out: { label: string; op: string; operands: string }[] = [];
  const lines = src.split(/\r?\n/);
  let cur: string | undefined;
  let inQuote = false;
  const flush = (): void => {
    if (cur === undefined) return;
    const m = /^(\S*)\s+(\S+)\s*(.*)$/s.exec(cur);
    if (m) out.push({ label: m[1]!, op: m[2]!, operands: m[3]! });
    cur = undefined;
  };
  for (const raw of lines) {
    if (cur === undefined && (raw.startsWith('*') || raw.trim() === '')) continue;
    const padded = raw.padEnd(72);
    const cont = padded[71] !== ' ';
    let body = cur === undefined ? padded.slice(0, 71) : padded.slice(15, 71);
    if (!inQuote) body = body.trimEnd();
    if (!cont && inQuote) body = body.trimEnd();
    cur = (cur ?? '') + body;
    // track open quotes ('' is an escaped quote and toggles twice)
    for (const ch of body) if (ch === "'") inQuote = !inQuote;
    if (!cont) {
      inQuote = false;
      flush();
    }
  }
  flush();
  return out;
}

/** Splits `A=x,B=(y,z),C='q,r'` into a key/value map. */
function operands(text: string): Map<string, string> {
  const res = new Map<string, string>();
  let i = 0;
  while (i < text.length) {
    const eq = text.indexOf('=', i);
    if (eq < 0) break;
    const key = text.slice(i, eq).trim();
    let j = eq + 1;
    let depth = 0;
    let q = false;
    for (; j < text.length; j++) {
      const ch = text[j];
      if (ch === "'") q = !q;
      else if (!q && ch === '(') depth++;
      else if (!q && ch === ')') depth--;
      else if (!q && depth === 0 && ch === ',') break;
      else if (!q && depth === 0 && ch === ' ') break;
    }
    res.set(key, text.slice(eq + 1, j));
    i = j + 1;
    if (text[j] === ' ') break;
  }
  return res;
}

const list = (v: string | undefined): string[] => (v ? v.replace(/^\(|\)$/g, '').split(',').filter(Boolean) : []);
const literal = (v: string): string => v.replace(/^'|'$/g, '').replace(/''/g, "'").replace(/&&/g, '&');

export function parseBms(src: string): GenMap[] {
  const maps: GenMap[] = [];
  let mapset = '';
  let ctrl: string[] = [];
  let extatt = false;
  let cur: GenMap | undefined;
  for (const st of statements(src)) {
    const ops = operands(st.operands);
    if (st.op === 'DFHMSD') {
      if (ops.get('TYPE') === 'FINAL') continue;
      mapset = st.label;
      ctrl = list(ops.get('CTRL'));
      extatt = ops.get('EXTATT') === 'YES';
    } else if (st.op === 'DFHMDI') {
      const size = list(ops.get('SIZE')).map(Number);
      cur = { mapset, map: st.label, size: [size[0] ?? 24, size[1] ?? 80], ctrl: [...ctrl, ...list(ops.get('CTRL'))], extatt, fields: [] };
      maps.push(cur);
    } else if (st.op === 'DFHMDF' && cur) {
      const pos = list(ops.get('POS')).map(Number);
      const f: GenField = { row: pos[0]!, col: pos[1]!, length: Number(ops.get('LENGTH') ?? 0), attrb: list(ops.get('ATTRB')) };
      if (st.label) f.name = st.label;
      if (ops.has('COLOR')) f.color = ops.get('COLOR')!;
      if (ops.has('HILIGHT')) f.hilight = ops.get('HILIGHT')!;
      if (ops.has('INITIAL')) f.initial = literal(ops.get('INITIAL')!);
      if (ops.has('PICOUT')) f.picout = literal(ops.get('PICOUT')!);
      if (ops.has('PICIN') && ops.get('PICIN')) f.picin = literal(ops.get('PICIN')!);
      if (ops.has('JUSTIFY')) f.justify = list(ops.get('JUSTIFY'));
      cur.fields.push(f);
    }
  }
  return maps;
}

/** Named input fields (`xxxI PIC X(n)`) of each 01 `<map>I` group in a symbolic-map copybook. */
const picLength = (pic: string): number =>
  pic.replace(/([X9])\((\d+)\)/g, (_m, c: string, n: string) => c.repeat(Number(n))).length;

export function parseSymbolicMap(src: string): Map<string, { name: string; length: number }[]> {
  const res = new Map<string, { name: string; length: number }[]>();
  let cur: { name: string; length: number }[] | undefined;
  for (const raw of src.split(/\r?\n/)) {
    const line = raw.slice(0, 72);
    if (/^\s{6}\*/.test(line)) continue;
    const m01 = /^\s+01\s+(\S+?)I\.\s*$/.exec(line);
    if (m01) {
      cur = [];
      res.set(m01[1]!, cur);
      continue;
    }
    if (/^\s+01\s/.test(line)) cur = undefined;
    const mf = /^\s+02\s+(\S+)I\s+PIC\s+([X9()0-9]+)\./.exec(line);
    if (mf && cur) cur.push({ name: mf[1]!, length: picLength(mf[2]!) });
  }
  return res;
}

export function generate(appDir: string): string {
  const bmsDir = join(appDir, 'bms');
  const cpyDir = join(appDir, 'cpy-bms');
  const all: GenMap[] = [];
  for (const file of readdirSync(bmsDir).filter((f) => f.endsWith('.bms')).sort()) {
    const maps = parseBms(readFileSync(join(bmsDir, file), 'latin1'));
    const sym = parseSymbolicMap(readFileSync(join(cpyDir, file.replace(/\.bms$/, '.CPY')), 'latin1'));
    for (const m of maps) {
      const named = m.fields.filter((f) => f.name).map((f) => ({ name: f.name!, length: f.length }));
      const expected = sym.get(m.map);
      if (!expected) throw new Error(`${file}: symbolic map ${m.map}I not found in cpy-bms`);
      const a = JSON.stringify(named);
      const b = JSON.stringify(expected);
      if (a !== b) throw new Error(`${file}: ${m.map} BMS fields differ from cpy-bms\n${a}\n${b}`);
      all.push(m);
    }
  }
  const body = all.map((m) => `  ${m.map}: ${JSON.stringify(m)},`).join('\n');
  return (
    '/**\n * BMS physical + symbolic map definitions generated from app/bms/*.bms and\n' +
    ' * cross-checked against app/cpy-bms/*.CPY by tools/gen-bms-maps.ts. Do not edit by hand.\n */\n' +
    "import type { MapDefinitionSource } from './types.js';\n\n" +
    'export const GENERATED_MAPS: Readonly<Record<string, MapDefinitionSource>> = {\n' +
    body +
    '\n};\n'
  );
}

const here = fileURLToPath(import.meta.url);
if (process.argv[1] && resolve(process.argv[1]) === here) {
  const ts = resolve(here, '..', '..');
  writeFileSync(join(ts, 'src/online/framework/bms/maps.generated.ts'), generate(resolve(ts, '..', 'app')));
  console.log('wrote src/online/framework/bms/maps.generated.ts');
}
