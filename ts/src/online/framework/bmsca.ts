/**
 * DFHBMSCA equivalents. Attribute constants are modelled as structured values
 * instead of 3270 attribute bytes; colours/highlights as their BMS keyword.
 */
export type Protection = 'UNPROT' | 'PROT' | 'ASKIP';
export type Intensity = 'NORM' | 'BRT' | 'DRK';

export interface FieldAttribute {
  protection: Protection;
  intensity: Intensity;
  numeric: boolean;
  /** Modified data tag (FSET). */
  fset: boolean;
}

const attr = (protection: Protection, intensity: Intensity, numeric = false, fset = false): FieldAttribute =>
  Object.freeze({ protection, intensity, numeric, fset });

/** DFHBMSCA attribute-byte constants (MOVE DFHBMxxx TO fieldA). */
export const DFHBMSCA = {
  DFHBMPEM: attr('UNPROT', 'NORM'),
  DFHBMUNP: attr('UNPROT', 'NORM'),
  DFHBMUNN: attr('UNPROT', 'NORM', true),
  DFHBMPRO: attr('PROT', 'NORM'),
  DFHBMBRY: attr('UNPROT', 'BRT'),
  DFHBMDAR: attr('UNPROT', 'DRK'),
  DFHBMFSE: attr('UNPROT', 'NORM', false, true),
  DFHBMPRF: attr('PROT', 'NORM', false, true),
  DFHBMASK: attr('ASKIP', 'NORM'),
  DFHBMASF: attr('ASKIP', 'NORM', false, true),
  DFHBMASB: attr('ASKIP', 'BRT'),
  DFHUNIMD: attr('UNPROT', 'BRT', false, true),
  DFHUNNUM: attr('UNPROT', 'NORM', true, true),
  DFHUNINT: attr('UNPROT', 'BRT', true, true),
  DFHUNNOD: attr('UNPROT', 'DRK', false, true),
  DFHPROTI: attr('PROT', 'BRT'),
  DFHPROTN: attr('PROT', 'DRK'),
} as const;

/** Extended colour constants (MOVE DFHRED TO fieldC). */
export const DFHCOLOR = {
  DFHDFCOL: 'DEFAULT',
  DFHBLUE: 'BLUE',
  DFHRED: 'RED',
  DFHPINK: 'PINK',
  DFHGREEN: 'GREEN',
  DFHTURQ: 'TURQUOISE',
  DFHYELLO: 'YELLOW',
  DFHNEUTR: 'NEUTRAL',
} as const;
export type Color = (typeof DFHCOLOR)[keyof typeof DFHCOLOR];

/** Extended highlighting constants (MOVE DFHUNDLN TO fieldH). */
export const DFHHILIGHT = {
  DFHDFHI: 'OFF',
  DFHBLINK: 'BLINK',
  DFHREVRS: 'REVERSE',
  DFHUNDLN: 'UNDERLINE',
} as const;
export type Hilight = (typeof DFHHILIGHT)[keyof typeof DFHHILIGHT];

/** Resolves a DFHMDF ATTRB=(...) list to a structured attribute (BMS defaults: ASKIP,NORM). */
export function attributeFromAttrb(attrb: readonly string[]): FieldAttribute {
  const has = (k: string): boolean => attrb.includes(k);
  const protection: Protection = has('UNPROT') ? 'UNPROT' : has('PROT') ? 'PROT' : 'ASKIP';
  const intensity: Intensity = has('BRT') ? 'BRT' : has('DRK') ? 'DRK' : 'NORM';
  return { protection, intensity, numeric: has('NUM'), fset: has('FSET') };
}
