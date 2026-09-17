/**
 * A parser for the BMS macro source in `app/bms`.
 *
 * BMS maps are assembler source: a statement starts in column 1 (optional
 * label) with the macro in the operation field, operands are separated by
 * commas, and a non blank in column 72 continues the statement in column 16 of
 * the next line. Only the three map macros matter here — `DFHMSD` (mapset),
 * `DFHMDI` (map) and `DFHMDF` (field).
 */

import type {
  BmsField,
  BmsMapset,
  FieldColor,
  FieldHighlight,
  FieldIntensity,
  ScreenDefinition,
} from "./types.js";

/** Column 72 holds the continuation indicator, so the operands end at 71. */
const CONTENT_END = 71;
/** A continued statement resumes in column 16. */
const CONTINUATION_START = 15;

const COLORS: ReadonlySet<string> = new Set([
  "blue",
  "green",
  "neutral",
  "red",
  "turquoise",
  "yellow",
  "default",
]);

const HIGHLIGHTS: ReadonlySet<string> = new Set(["off", "underline", "blink", "reverse"]);

export interface BmsStatement {
  readonly label?: string;
  readonly macro: string;
  readonly operands: ReadonlyMap<string, string>;
}

export class BmsParseError extends Error {}

/**
 * Folds the continuation lines of `source` into one string per statement,
 * dropping comment (`*` in column 1) and blank lines.
 */
export function joinContinuations(source: string): string[] {
  const statements: string[] = [];
  let pending: string | undefined;

  for (const line of source.split(/\r?\n/)) {
    if (line.startsWith("*") || line.trim().length === 0) {
      continue;
    }

    const content = line.slice(0, CONTENT_END);
    const continued = line.length > CONTENT_END && line.charAt(CONTENT_END) !== " ";

    pending = pending === undefined ? content : pending + content.slice(CONTINUATION_START);

    if (!continued) {
      statements.push(pending);
      pending = undefined;
    }
  }

  if (pending !== undefined) {
    statements.push(pending);
  }

  return statements;
}

/** Splits an operand list on the commas that are outside quotes and parentheses. */
function splitOperands(text: string): string[] {
  const operands: string[] = [];
  let current = "";
  let depth = 0;
  let quoted = false;

  for (let index = 0; index < text.length; index += 1) {
    const char = text.charAt(index);

    if (quoted) {
      if (char === "'" && text.charAt(index + 1) === "'") {
        current += "''";
        index += 1;
        continue;
      }
      if (char === "'") {
        quoted = false;
      }
      current += char;
      continue;
    }

    if (char === "'") {
      quoted = true;
      current += char;
      continue;
    }
    if (char === "(") {
      depth += 1;
    } else if (char === ")") {
      depth -= 1;
    }
    if (char === "," && depth === 0) {
      operands.push(current);
      current = "";
      continue;
    }
    if (char === " " && depth === 0 && !quoted) {
      continue;
    }
    current += char;
  }

  if (current.trim().length > 0) {
    operands.push(current);
  }

  return operands.map((operand) => operand.trim()).filter((operand) => operand.length > 0);
}

/** `INITIAL='it''s'` holds a doubled quote for every literal quote. */
function unquote(value: string): string {
  if (!value.startsWith("'")) {
    return value;
  }
  return value.slice(1, -1).replaceAll("''", "'");
}

function parseStatement(text: string): BmsStatement | undefined {
  const match = /^(\S*)\s+(DFHMSD|DFHMDI|DFHMDF)\s*(.*)$/.exec(text.trimEnd());
  if (match === null) {
    return undefined;
  }

  const [, label = "", macro = "", rest = ""] = match;
  const operands = new Map<string, string>();

  for (const operand of splitOperands(rest)) {
    const separator = operand.indexOf("=");
    if (separator === -1) {
      operands.set(operand, "");
      continue;
    }
    operands.set(operand.slice(0, separator), operand.slice(separator + 1));
  }

  return label.length > 0 ? { label, macro, operands } : { macro, operands };
}

/** Strips the parentheses of a sublist operand such as `ATTRB=(ASKIP,NORM)`. */
function sublist(value: string | undefined): string[] {
  if (value === undefined) {
    return [];
  }
  const inner = value.startsWith("(") ? value.slice(1, -1) : value;
  return inner
    .split(",")
    .map((item) => item.trim())
    .filter((item) => item.length > 0);
}

function requireNumber(operands: ReadonlyMap<string, string>, key: string, macro: string): number {
  const raw = operands.get(key);
  if (raw === undefined) {
    throw new BmsParseError(`${macro} is missing ${key}`);
  }
  const value = Number(raw);
  if (!Number.isInteger(value)) {
    throw new BmsParseError(`${macro} has a non numeric ${key}: ${raw}`);
  }
  return value;
}

function parsePosition(operands: ReadonlyMap<string, string>): { row: number; column: number } {
  const [row, column] = sublist(operands.get("POS")).map(Number);
  if (row === undefined || column === undefined || !Number.isInteger(row) || !Number.isInteger(column)) {
    throw new BmsParseError(`DFHMDF has an unreadable POS: ${operands.get("POS") ?? "(absent)"}`);
  }
  return { row, column };
}

function parseColor(value: string | undefined): FieldColor | undefined {
  if (value === undefined) {
    return undefined;
  }
  const color = value.toLowerCase();
  return COLORS.has(color) ? (color as FieldColor) : undefined;
}

function parseHighlight(value: string | undefined): FieldHighlight | undefined {
  if (value === undefined) {
    return undefined;
  }
  const highlight = value.toLowerCase();
  return HIGHLIGHTS.has(highlight) ? (highlight as FieldHighlight) : undefined;
}

function parseField(statement: BmsStatement): BmsField {
  const { operands } = statement;
  const attributes = new Set(sublist(operands.get("ATTRB")));
  const { row, column } = parsePosition(operands);

  const autoSkip = attributes.has("ASKIP") || attributes.size === 0;
  const intensity: FieldIntensity = attributes.has("BRT")
    ? "bright"
    : attributes.has("DRK")
      ? "dark"
      : "normal";

  const initial = operands.get("INITIAL");
  const picin = operands.get("PICIN");
  const picout = operands.get("PICOUT");
  const color = parseColor(operands.get("COLOR"));
  const highlight = parseHighlight(operands.get("HILIGHT"));

  return {
    ...(statement.label === undefined ? {} : { name: statement.label }),
    row,
    column,
    length: requireNumber(operands, "LENGTH", "DFHMDF"),
    protected: autoSkip || attributes.has("PROT") || !attributes.has("UNPROT"),
    autoSkip,
    numeric: attributes.has("NUM"),
    intensity,
    initialCursor: attributes.has("IC"),
    fset: attributes.has("FSET"),
    mustFill: sublist(operands.get("VALIDN")).includes("MUSTFILL"),
    rightJustify: sublist(operands.get("JUSTIFY")).includes("RIGHT"),
    ...(color === undefined ? {} : { color }),
    ...(highlight === undefined ? {} : { highlight }),
    ...(initial === undefined ? {} : { initial: unquote(initial) }),
    ...(picin === undefined ? {} : { picin: unquote(picin) }),
    ...(picout === undefined ? {} : { picout: unquote(picout) }),
  };
}

/**
 * Parses one `.bms` source file into its mapset and maps.
 *
 * A field with no `ATTRB=` takes the BMS default of an autoskip field at normal
 * intensity, and `POS` is the position of the attribute byte, so the field data
 * starts one column to its right.
 */
export function parseBms(source: string): BmsMapset {
  let mapsetName: string | undefined;
  const maps: ScreenDefinition[] = [];
  let currentMap: { map: string; rows: number; columns: number; fields: BmsField[] } | undefined;

  const close = (): void => {
    if (currentMap !== undefined && mapsetName !== undefined) {
      maps.push({ mapset: mapsetName, ...currentMap });
      currentMap = undefined;
    }
  };

  for (const text of joinContinuations(source)) {
    const statement = parseStatement(text);
    if (statement === undefined) {
      continue;
    }

    if (statement.macro === "DFHMSD") {
      if (statement.label !== undefined) {
        mapsetName = statement.label;
      }
      if (statement.operands.get("TYPE") === "FINAL") {
        close();
      }
      continue;
    }

    if (statement.macro === "DFHMDI") {
      close();
      if (statement.label === undefined) {
        throw new BmsParseError("DFHMDI without a map name");
      }
      const [rows, columns] = sublist(statement.operands.get("SIZE")).map(Number);
      currentMap = {
        map: statement.label,
        rows: rows ?? 24,
        columns: columns ?? 80,
        fields: [],
      };
      continue;
    }

    if (currentMap === undefined) {
      throw new BmsParseError("DFHMDF outside of a map");
    }
    currentMap.fields.push(parseField(statement));
  }

  close();

  if (mapsetName === undefined) {
    throw new BmsParseError("no DFHMSD mapset in the source");
  }

  return { name: mapsetName, maps };
}
