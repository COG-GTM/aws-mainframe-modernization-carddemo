/**
 * Record images: the fixed-width form a COBOL `DISPLAY` of a group item or of
 * one of its elementary fields produces, including zoned sign overpunches.
 */

import { findField } from "@carddemo/copybook";
import type { RecordCodec } from "@carddemo/domain";

export interface RecordImage {
  readonly line: string;
  field(name: string): string;
}

export function recordImage<T>(codec: RecordCodec<T>, record: T): RecordImage {
  const line = codec.encode(record);
  return {
    line,
    field(name: string): string {
      const field = findField(codec.layout, name);
      return line.slice(field.offset, field.offset + field.width);
    },
  };
}
