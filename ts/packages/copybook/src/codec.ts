/**
 * Fixed-width record codec driven by a {@link Layout}.
 *
 * Records shorter than the layout are padded with spaces, matching the way the
 * sample data files drop trailing `FILLER`.
 */

import type { Field, Layout } from "./layout.js";
import { decodePacked, decodeZoned, encodePacked, encodeZoned } from "./zoned.js";

export type FieldValue = string | number;
export type RecordValue = FieldValue | readonly FieldValue[];
export type DecodedRecord = Record<string, RecordValue>;

const LATIN1 = "latin1";

function decodeElement(field: Field, raw: string): FieldValue {
  if (field.pic.category === "alphanumeric") {
    return raw.replace(/ +$/, "");
  }

  const options = {
    digits: field.pic.integerDigits + field.pic.decimalDigits,
    scale: field.pic.decimalDigits,
    signed: field.pic.signed,
  };

  if (field.usage === "comp-3") {
    return decodePacked(Uint8Array.from(Buffer.from(raw, LATIN1)), options);
  }
  return decodeZoned(raw, options);
}

function encodeElement(field: Field, value: FieldValue | undefined): string {
  if (field.pic.category === "alphanumeric") {
    const text = value === undefined ? "" : String(value);
    if (text.length > field.elementWidth) {
      throw new Error(`value for ${field.name ?? "FILLER"} exceeds PIC X(${field.elementWidth})`);
    }
    return text.padEnd(field.elementWidth, " ");
  }

  const options = {
    digits: field.pic.integerDigits + field.pic.decimalDigits,
    scale: field.pic.decimalDigits,
    signed: field.pic.signed,
  };
  const numeric = value === undefined ? 0 : Number(value);

  if (field.usage === "comp-3") {
    return Buffer.from(encodePacked(numeric, options)).toString(LATIN1);
  }
  return encodeZoned(numeric, options);
}

export function decodeRecord(layout: Layout, line: string): DecodedRecord {
  const padded = line.padEnd(layout.recordLength, " ");
  const record: DecodedRecord = {};

  for (const field of layout.fields) {
    if (field.name === undefined) {
      continue;
    }
    if (field.occurs > 1) {
      const values: FieldValue[] = [];
      for (let index = 0; index < field.occurs; index += 1) {
        const start = field.offset + index * field.elementWidth;
        values.push(decodeElement(field, padded.slice(start, start + field.elementWidth)));
      }
      record[field.name] = values;
    } else {
      record[field.name] = decodeElement(
        field,
        padded.slice(field.offset, field.offset + field.width),
      );
    }
  }

  return record;
}

export function encodeRecord(layout: Layout, record: DecodedRecord): string {
  let out = "";

  for (const field of layout.fields) {
    const value = field.name === undefined ? undefined : record[field.name];
    if (field.occurs > 1) {
      const values = Array.isArray(value) ? (value as readonly FieldValue[]) : [];
      for (let index = 0; index < field.occurs; index += 1) {
        out += encodeElement(field, values[index]);
      }
    } else {
      out += encodeElement(field, value as FieldValue | undefined);
    }
  }

  return out;
}
