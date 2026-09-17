/**
 * Typed screen definitions produced from the `DFHMSD`/`DFHMDI`/`DFHMDF` macros
 * in `app/bms`. One BMS map becomes one {@link ScreenDefinition}; the fields
 * keep the BMS geometry, so a renderer can place them on the 24x80 grid the
 * 3270 terminal used.
 */

/** `COLOR=` on `DFHMDF`, lower cased. */
export type FieldColor = "blue" | "green" | "neutral" | "red" | "turquoise" | "yellow" | "default";

/** `HILIGHT=` on `DFHMDF`, lower cased. */
export type FieldHighlight = "off" | "underline" | "blink" | "reverse";

/** The intensity part of `ATTRB=`: `NORM`, `BRT` or `DRK`. */
export type FieldIntensity = "normal" | "bright" | "dark";

export interface BmsField {
  /** The `DFHMDF` label, absent for the literal fields BMS leaves unnamed. */
  readonly name?: string;
  /** Row of the attribute byte, 1 based. */
  readonly row: number;
  /** Column of the attribute byte, 1 based; the data starts one column later. */
  readonly column: number;
  /** `LENGTH=`; `0` for the stopper fields that only carry an attribute byte. */
  readonly length: number;
  /** `PROT`/`ASKIP` field, or the BMS default when `ATTRB=` is absent. */
  readonly protected: boolean;
  /** `ASKIP`: protected and skipped over by the cursor. */
  readonly autoSkip: boolean;
  /** `NUM`: numeric shift. */
  readonly numeric: boolean;
  readonly intensity: FieldIntensity;
  /** `IC`: the field the cursor starts in. */
  readonly initialCursor: boolean;
  /** `FSET`: modified data tag preset, so the field is always transmitted. */
  readonly fset: boolean;
  /** `VALIDN=(MUSTFILL)`. */
  readonly mustFill: boolean;
  /** `JUSTIFY=(RIGHT)`. */
  readonly rightJustify: boolean;
  readonly color?: FieldColor;
  readonly highlight?: FieldHighlight;
  /** `INITIAL=`, with the BMS `''` escape resolved to a single quote. */
  readonly initial?: string;
  /** `PICIN=`. */
  readonly picin?: string;
  /** `PICOUT=`. */
  readonly picout?: string;
}

export interface ScreenDefinition {
  /** The `DFHMSD` mapset name, e.g. `COSGN00`. */
  readonly mapset: string;
  /** The `DFHMDI` map name, e.g. `COSGN0A`. */
  readonly map: string;
  /** `SIZE=(rows,columns)` on `DFHMDI`. */
  readonly rows: number;
  readonly columns: number;
  readonly fields: readonly BmsField[];
}

export interface BmsMapset {
  readonly name: string;
  readonly maps: readonly ScreenDefinition[];
}
