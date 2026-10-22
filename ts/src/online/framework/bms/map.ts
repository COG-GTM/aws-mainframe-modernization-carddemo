import { attributeFromAttrb, type Color, type FieldAttribute, type Hilight } from '../bmsca.js';
import { GENERATED_MAPS } from './maps.generated.js';
import type { FieldDefinition, MapDefinitionSource } from './types.js';

export const LOW_VALUE = '\u0000';
export const lowValues = (n: number): string => LOW_VALUE.repeat(n);

export interface MapDefinition extends MapDefinitionSource {
  /** Named fields by DFHMDF label. */
  byName: ReadonlyMap<string, FieldDefinition>;
}

const cache = new Map<string, MapDefinition>();

/** Looks up a generated map by MAP name (e.g. 'COSGN0A'), optionally checking the MAPSET. */
export function getMapDefinition(map: string, mapset?: string): MapDefinition {
  const key = map.trim();
  let def = cache.get(key);
  if (!def) {
    const src = GENERATED_MAPS[key];
    if (!src) throw new Error(`BMS map ${key} is not defined`);
    const byName = new Map<string, FieldDefinition>();
    for (const f of src.fields) if (f.name) byName.set(f.name, f);
    def = { ...src, byName };
    cache.set(key, def);
  }
  if (mapset !== undefined && def.mapset !== mapset.trim()) throw new Error(`map ${key} is not in mapset ${mapset}`);
  return def;
}

export const mapNames = (): string[] => Object.keys(GENERATED_MAPS);

/** One named field of a symbolic map: the xxxL / xxxF / xxxA / xxxC / xxxH / xxxI|xxxO slots. */
export interface SymbolicField {
  /** xxxL: received length; MOVE -1 sets symbolic cursor positioning. */
  length: number;
  /** xxxF: X'80' when the field was cleared (modified, no data). */
  flag: number;
  /** xxxA: attribute override; undefined models X'00' (use the map default). */
  attr: FieldAttribute | undefined;
  color: Color | undefined;
  hilight: Hilight | undefined;
  /** xxxI / xxxO (they share storage); always exactly the field length. */
  data: string;
}

/**
 * Symbolic map (the copybook generated into app/cpy-bms/). A fresh map models
 * WORKING-STORAGE with no VALUE clauses, i.e. LOW-VALUES throughout.
 */
export class SymbolicMap {
  readonly def: MapDefinition;
  private readonly slots = new Map<string, SymbolicField>();

  constructor(map: string, mapset?: string) {
    this.def = getMapDefinition(map, mapset);
    this.lowValues();
  }

  get name(): string {
    return this.def.map;
  }

  /** MOVE LOW-VALUES TO xxxO (clears every L/F/A/C/H/data slot). */
  lowValues(): this {
    for (const [name, f] of this.def.byName) {
      this.slots.set(name, { length: 0, flag: 0, attr: undefined, color: undefined, hilight: undefined, data: lowValues(f.length) });
    }
    return this;
  }

  field(name: string): SymbolicField {
    const s = this.slots.get(name);
    if (!s) throw new Error(`map ${this.def.map} has no field ${name}`);
    return s;
  }

  fieldLength(name: string): number {
    return this.def.byName.get(name)?.length ?? this.field(name).data.length;
  }

  /** xxxI / xxxO value, exactly the field length (may contain LOW-VALUES). */
  get(name: string): string {
    return this.field(name).data;
  }

  /** Alphanumeric MOVE into xxxO: left-justified, space-padded, truncated. */
  set(name: string, value: string): this {
    const n = this.fieldLength(name);
    this.field(name).data = value.length >= n ? value.slice(0, n) : value + ' '.repeat(n - value.length);
    return this;
  }

  /** Raw MOVE of a full-length value (e.g. LOW-VALUES) into xxxO. */
  setRaw(name: string, value: string): this {
    const n = this.fieldLength(name);
    this.field(name).data = value.padEnd(n, LOW_VALUE).slice(0, n);
    return this;
  }

  getLength(name: string): number {
    return this.field(name).length;
  }

  /** MOVE n TO xxxL (MOVE -1 requests the cursor on SEND MAP ... CURSOR). */
  setLength(name: string, n: number): this {
    this.field(name).length = n;
    return this;
  }

  setAttr(name: string, attr: FieldAttribute | undefined): this {
    this.field(name).attr = attr;
    return this;
  }

  setColor(name: string, color: Color | undefined): this {
    this.field(name).color = color;
    return this;
  }

  setHilight(name: string, hilight: Hilight | undefined): this {
    this.field(name).hilight = hilight;
    return this;
  }

  fieldNames(): string[] {
    return [...this.def.byName.keys()];
  }

  /** Default ATTRB of a named field. */
  defaultAttr(name: string): FieldAttribute {
    const f = this.def.byName.get(name);
    if (!f) throw new Error(`map ${this.def.map} has no field ${name}`);
    return attributeFromAttrb(f.attrb);
  }
}
