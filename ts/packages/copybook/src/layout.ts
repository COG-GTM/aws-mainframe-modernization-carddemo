/**
 * Record layouts: the TypeScript equivalent of a COBOL `01` level structure.
 *
 * A layout is an ordered list of fields; every field knows its own byte width so
 * a record can be decoded from, and encoded back to, its fixed-width form.
 */

import { parsePic, type PicClause } from "./pic.js";
import { packedLength } from "./zoned.js";

export type Usage = "display" | "comp-3";

export interface FieldSpec {
  /** Property name on the decoded record; `undefined` for `FILLER`. */
  readonly name?: string;
  readonly pic: string;
  readonly usage?: Usage;
  /** `OCCURS n TIMES`; the decoded value is an array of length `n`. */
  readonly occurs?: number;
}

export interface Field {
  readonly name: string | undefined;
  readonly pic: PicClause;
  readonly usage: Usage;
  readonly occurs: number;
  /** Width of a single element in characters. */
  readonly elementWidth: number;
  readonly offset: number;
  /** Width of the whole field, including every `OCCURS` element. */
  readonly width: number;
}

export interface Layout {
  readonly name: string;
  readonly fields: readonly Field[];
  readonly recordLength: number;
}

function elementWidth(pic: PicClause, usage: Usage): number {
  if (usage === "comp-3") {
    return packedLength(pic.integerDigits + pic.decimalDigits);
  }
  return pic.length;
}

export function defineLayout(name: string, specs: readonly FieldSpec[]): Layout {
  let offset = 0;
  const fields = specs.map((spec) => {
    const pic = parsePic(spec.pic);
    const usage = spec.usage ?? "display";
    const occurs = spec.occurs ?? 1;
    const singleWidth = elementWidth(pic, usage);
    const width = singleWidth * occurs;
    const field: Field = {
      name: spec.name,
      pic,
      usage,
      occurs,
      elementWidth: singleWidth,
      offset,
      width,
    };
    offset += width;
    return field;
  });

  return { name, fields, recordLength: offset };
}

export function findField(layout: Layout, name: string): Field {
  const field = layout.fields.find((candidate) => candidate.name === name);
  if (field === undefined) {
    throw new Error(`field ${name} is not part of layout ${layout.name}`);
  }
  return field;
}
