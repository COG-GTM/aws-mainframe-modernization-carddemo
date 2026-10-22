/** Shapes of the generated BMS map definitions (see tools/gen-bms-maps.ts). */
export interface FieldDefinition {
  /** DFHMDF label; absent for unnamed (literal / stopper) fields. */
  name?: string;
  /** 1-based POS=(row,col) of the field data (the attribute byte is at col-1). */
  row: number;
  col: number;
  length: number;
  /** ATTRB operands, e.g. ['ASKIP','FSET','NORM']. */
  attrb: string[];
  color?: string;
  hilight?: string;
  initial?: string;
  picout?: string;
  picin?: string;
  justify?: string[];
}

export interface MapDefinitionSource {
  mapset: string;
  map: string;
  size: [number, number];
  ctrl: string[];
  extatt: boolean;
  fields: FieldDefinition[];
}
