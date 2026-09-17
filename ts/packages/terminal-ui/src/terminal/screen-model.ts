/**
 * Turns a {@link ScreenDefinition} into what the renderer needs: the field
 * placement on the 24x80 grid, the input fields in BMS order (which is the
 * tab order the 3270 used) and a plain text view of the screen for tests.
 */

import type { BmsField, ScreenDefinition } from "../bms/types.js";

/** Field values keyed by the `DFHMDF` label, as sent to and from the program. */
export type ScreenFields = Readonly<Record<string, string>>;

export interface PlacedField {
  /** Stable key: the field name, or its position for the unnamed literals. */
  readonly key: string;
  readonly field: BmsField;
  /** Column of the first data character, 1 based (`POS` is the attribute byte). */
  readonly dataColumn: number;
  /** Data width after clipping to the right edge of the screen. */
  readonly width: number;
  /** Unprotected fields the operator can type into. */
  readonly editable: boolean;
}

export function fieldKey(field: BmsField): string {
  return field.name ?? `@${String(field.row)}-${String(field.column)}`;
}

/** The fields of a screen with their data position resolved. */
export function placeFields(screen: ScreenDefinition): PlacedField[] {
  return screen.fields.map((field) => {
    const dataColumn = field.column + 1;
    const width = Math.max(0, Math.min(field.length, screen.columns - dataColumn + 1));
    return {
      key: fieldKey(field),
      field,
      dataColumn,
      width,
      editable: !field.protected && field.length > 0,
    };
  });
}

/** The named input fields in BMS order, which is the order the cursor tabs through. */
export function inputFields(screen: ScreenDefinition): BmsField[] {
  return screen.fields.filter((field) => !field.protected && field.length > 0 && field.name !== undefined);
}

/** The field the `IC` attribute puts the cursor in, if the map sets one. */
export function initialCursorField(screen: ScreenDefinition): BmsField | undefined {
  return screen.fields.find((field) => field.initialCursor && field.name !== undefined);
}

/** The values a freshly sent map carries: its `INITIAL=` text per named field. */
export function initialValues(screen: ScreenDefinition): Record<string, string> {
  const values: Record<string, string> = {};
  for (const field of screen.fields) {
    if (field.name !== undefined) {
      values[field.name] = field.initial ?? "";
    }
  }
  return values;
}

/** The value shown in a field: the program's value, else the map's `INITIAL=`. */
export function displayValue(field: BmsField, values: ScreenFields): string {
  const value = field.name === undefined ? undefined : values[field.name];
  return (value ?? field.initial ?? "").slice(0, field.length);
}

/**
 * Renders the screen as `rows` lines of `columns` characters, the way the
 * buffer would look on the terminal. Dark fields contribute blanks.
 */
export function screenText(screen: ScreenDefinition, values: ScreenFields = {}): string[] {
  const buffer = Array.from({ length: screen.rows }, () => new Array<string>(screen.columns).fill(" "));

  for (const placed of placeFields(screen)) {
    const row = buffer[placed.field.row - 1];
    if (row === undefined || placed.width === 0) {
      continue;
    }
    const text = placed.field.intensity === "dark" ? "" : displayValue(placed.field, values);
    for (let offset = 0; offset < placed.width; offset += 1) {
      row[placed.dataColumn - 1 + offset] = text.charAt(offset) === "" ? " " : text.charAt(offset);
    }
  }

  return buffer.map((row) => row.join(""));
}
