import { attributeFromAttrb, type Color, type FieldAttribute, type Hilight } from '../bmsca.js';
import { isShortReadAid, type AidKey } from '../aid.js';
import { LOW_VALUE, lowValues, type SymbolicMap } from './map.js';

export const SCREEN_ROWS = 24;
export const SCREEN_COLS = 80;

/** A field as it currently exists on the (simulated) 3270 screen. */
export interface ScreenField {
  name?: string;
  row: number;
  col: number;
  length: number;
  /** Field content, exactly `length` chars; LOW-VALUES are nulls (shown as blanks). */
  value: string;
  attr: FieldAttribute;
  color?: Color;
  hilight?: Hilight;
  /** Modified data tag. */
  mdt: boolean;
  justify?: string[];
}

export interface Screen {
  map?: string;
  mapset?: string;
  fields: ScreenField[];
  /** SEND TEXT output (unformatted screen). */
  text?: string;
  cursor: { row: number; col: number };
}

export const emptyScreen = (): Screen => ({ fields: [], cursor: { row: 1, col: 1 } });

export interface SendMapOptions {
  erase?: boolean;
  dataonly?: boolean;
  maponly?: boolean;
  /** true = symbolic cursor positioning (first field with L = -1); number = buffer offset. */
  cursor?: boolean | number;
}

export class TerminalInputError extends Error {}

const fit = (s: string, n: number, pad = ' '): string => (s.length >= n ? s.slice(0, n) : s + pad.repeat(n - s.length));

function cursorAt(fields: readonly ScreenField[], sym: SymbolicMap, cursor: boolean | number | undefined): { row: number; col: number } {
  if (typeof cursor === 'number') return { row: Math.floor(cursor / SCREEN_COLS) + 1, col: (cursor % SCREEN_COLS) + 1 };
  if (cursor === true) {
    for (const f of fields) if (f.name && sym.getLength(f.name) === -1) return { row: f.row, col: f.col };
  }
  for (const f of sym.def.fields) if (f.attrb.includes('IC')) return { row: f.row, col: f.col };
  return { row: 1, col: 1 };
}

/** EXEC CICS SEND MAP(...) MAPSET(...) FROM(sym) [ERASE] [DATAONLY|MAPONLY] [CURSOR]. */
export function sendMap(screen: Screen, sym: SymbolicMap, opts: SendMapOptions = {}): Screen {
  const def = sym.def;
  if (opts.dataonly && screen.map === def.map) {
    const fields = screen.fields.map((f) => {
      if (!f.name) return f;
      const s = sym.field(f.name);
      const next = { ...f };
      if (s.data[0] !== LOW_VALUE) next.value = s.data;
      if (s.attr) {
        next.attr = s.attr;
        next.mdt = s.attr.fset;
      }
      if (s.color) next.color = s.color;
      if (s.hilight) next.hilight = s.hilight;
      return next;
    });
    return { map: def.map, mapset: def.mapset, fields, cursor: cursorAt(fields, sym, opts.cursor) };
  }
  // ERASE, or a full map write (without ERASE the new map overlays and replaces the old one).
  const fields: ScreenField[] = def.fields.map((f) => {
    const s = f.name && !opts.maponly ? sym.field(f.name) : undefined;
    const attr = s?.attr ?? attributeFromAttrb(f.attrb);
    const initial = f.initial !== undefined ? fit(f.initial, f.length) : lowValues(f.length);
    const field: ScreenField = {
      row: f.row,
      col: f.col,
      length: f.length,
      value: s && s.data[0] !== LOW_VALUE ? s.data : initial,
      attr,
      mdt: attr.fset,
    };
    if (f.name) field.name = f.name;
    const color = s?.color ?? (f.color as Color | undefined);
    if (color) field.color = color;
    const hilight = s?.hilight ?? (f.hilight as Hilight | undefined);
    if (hilight) field.hilight = hilight;
    if (f.justify) field.justify = f.justify;
    return field;
  });
  return { map: def.map, mapset: def.mapset, fields, cursor: cursorAt(fields, sym, opts.cursor) };
}

/** EXEC CICS SEND TEXT FROM(text) [ERASE]: unformatted screen. */
export function sendText(_screen: Screen, text: string): Screen {
  return { fields: [], text, cursor: { row: 1, col: 1 } };
}

/**
 * Applies operator input to the screen as a 3270 would: CLEAR erases the screen,
 * PA keys transmit nothing, otherwise each typed field replaces the field content
 * (unfilled positions are nulls, as after ERASE EOF) and sets its MDT.
 */
export function applyInput(screen: Screen, aid: AidKey, input: Readonly<Record<string, string>> = {}): Screen {
  if (aid === 'CLEAR') return emptyScreen();
  if (isShortReadAid(aid)) return screen;
  const fields = screen.fields.map((f) => ({ ...f }));
  for (const [name, raw] of Object.entries(input)) {
    const f = fields.find((x) => x.name === name);
    if (!f) throw new TerminalInputError(`field ${name} is not on the screen`);
    if (f.attr.protection !== 'UNPROT') throw new TerminalInputError(`field ${name} is protected`);
    const value = raw.slice(0, f.length);
    if (f.attr.numeric && /[^0-9.\- ]/.test(value)) throw new TerminalInputError(`field ${name} is numeric-only`);
    f.value = value.padEnd(f.length, LOW_VALUE);
    f.mdt = true;
  }
  return { ...screen, fields };
}

export const MAPFAIL = 36;

/**
 * EXEC CICS RECEIVE MAP(...) INTO(sym) RESP(...). Returns the RESP code.
 * Modified fields arrive with nulls suppressed, justified/padded per JUSTIFY
 * (default LEFT,BLANK); unmodified fields are set to LOW-VALUES with L = 0.
 * CLEAR/PA keys or no modified fields => MAPFAIL and the map is left untouched.
 */
export function receiveMap(screen: Screen, sym: SymbolicMap, aid: AidKey): number {
  if (isShortReadAid(aid) || screen.map !== sym.def.map) return MAPFAIL;
  const modified = new Map<string, ScreenField>();
  for (const f of screen.fields) if (f.name && f.mdt) modified.set(f.name, f);
  if (modified.size === 0) return MAPFAIL;
  for (const name of sym.fieldNames()) {
    const slot = sym.field(name);
    slot.attr = undefined;
    slot.color = undefined;
    slot.hilight = undefined;
    const f = modified.get(name);
    const len = sym.fieldLength(name);
    if (!f) {
      slot.length = 0;
      slot.flag = 0;
      slot.data = lowValues(len);
      continue;
    }
    const data = f.value.split(LOW_VALUE).join('');
    if (data.length === 0) {
      slot.length = 0;
      slot.flag = 0x80;
      slot.data = lowValues(len);
      continue;
    }
    const just = f.justify ?? [];
    const pad = just.includes('ZERO') ? '0' : ' ';
    slot.length = data.length;
    slot.flag = 0;
    slot.data = just.includes('RIGHT') ? data.padStart(len, pad) : data.padEnd(len, pad);
  }
  return 0;
}

/** Renders the screen as 24 lines of 80 characters (attribute bytes and DRK fields show as blanks). */
export function renderScreen(screen: Screen): string[] {
  const grid = Array.from({ length: SCREEN_ROWS }, () => Array<string>(SCREEN_COLS).fill(' '));
  if (screen.text !== undefined) {
    const t = screen.text.replace(/\0/g, ' ');
    for (let i = 0; i < t.length && i < SCREEN_ROWS * SCREEN_COLS; i++) grid[Math.floor(i / SCREEN_COLS)]![i % SCREEN_COLS] = t[i]!;
  }
  for (const f of screen.fields) {
    const shown = f.attr.intensity === 'DRK' ? ' '.repeat(f.length) : f.value;
    let pos = (f.row - 1) * SCREEN_COLS + (f.col - 1);
    for (const ch of shown) {
      if (pos >= SCREEN_ROWS * SCREEN_COLS) break;
      // Nulls are never transmitted, so they leave whatever an earlier field put there (e.g. the unnamed
      // POS=(1,1) LENGTH=9 field at the end of COACTVW/COACTUP must not blank 'Tran: CAV').
      if (ch !== LOW_VALUE) grid[Math.floor(pos / SCREEN_COLS)]![pos % SCREEN_COLS] = ch;
      pos++;
    }
  }
  return grid.map((r) => r.join(''));
}

/** JSON-friendly view of the screen for the HTTP API / tests. */
export interface ScreenSnapshot {
  map: string | null;
  mapset: string | null;
  cursor: { row: number; col: number };
  fields: {
    name: string | null;
    row: number;
    col: number;
    length: number;
    value: string;
    protection: string;
    intensity: string;
    numeric: boolean;
    color: string | null;
    hilight: string | null;
  }[];
  lines: string[];
}

export function snapshotScreen(screen: Screen): ScreenSnapshot {
  return {
    map: screen.map ?? null,
    mapset: screen.mapset ?? null,
    cursor: screen.cursor,
    fields: screen.fields.map((f) => ({
      name: f.name ?? null,
      row: f.row,
      col: f.col,
      length: f.length,
      // DRK (non-display) fields such as PASSWD are never exposed to clients.
      value: f.attr.intensity === 'DRK' ? ' '.repeat(f.length) : f.value.replace(/\0/g, ' '),
      protection: f.attr.protection,
      intensity: f.attr.intensity,
      numeric: f.attr.numeric,
      color: f.color ?? null,
      hilight: f.hilight ?? null,
    })),
    lines: renderScreen(screen),
  };
}
