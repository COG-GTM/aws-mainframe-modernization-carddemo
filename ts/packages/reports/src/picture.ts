/**
 * COBOL `MOVE` semantics for report fields: numeric-edited PICTURE formatting,
 * display-numeric moves into alphanumeric items and `STRING ... DELIMITED BY`.
 */

/** Expands the `Z(n)` / `9(n)` repeat notation into individual symbols. */
function expandPicture(picture: string): string {
  return picture.replace(/([Z9])\((\d+)\)/g, (_match, symbol: string, count: string) =>
    symbol.repeat(Number(count)),
  );
}

/** Scaled integer digits of `magnitude`, truncated as a COBOL `MOVE` does. */
function scaledDigits(magnitude: number, decimals: number, positions: number): string {
  const scaled = Math.trunc(Number((magnitude * 10 ** decimals).toFixed(6)));
  return String(scaled).padStart(positions, "0").slice(-positions);
}

function signCharacter(symbol: string, negative: boolean): string {
  if (negative) {
    return "-";
  }
  return symbol === "+" ? "+" : " ";
}

/**
 * Formats `value` into a numeric-edited PICTURE such as `-ZZZ,ZZZ,ZZZ.ZZ`,
 * `+ZZZ,ZZZ,ZZZ.ZZ`, `Z(9).99-` or `9(9).99-`.
 *
 * Leading zeros are replaced by spaces across `Z` positions (including the
 * insertion commas inside the suppressed run), suppression stops at the decimal
 * point, and a value of zero blanks the whole item when every digit position is
 * suppressible. High-order digits that do not fit are truncated.
 */
export function formatEdited(picture: string, value: number): string {
  const expanded = expandPicture(picture);
  const leadingSign = expanded.startsWith("-") || expanded.startsWith("+") ? expanded[0] : undefined;
  const trailingSign = expanded.endsWith("-") || expanded.endsWith("+") ? expanded.at(-1) : undefined;
  const body = expanded.slice(
    leadingSign === undefined ? 0 : 1,
    trailingSign === undefined ? undefined : -1,
  );

  const digitPositions = [...body].filter((symbol) => symbol === "Z" || symbol === "9").length;
  const decimals = body.includes(".") ? body.length - body.indexOf(".") - 1 : 0;
  const negative = value < 0;
  const digits = scaledDigits(Math.abs(value), decimals, digitPositions);

  const allSuppressible = !body.includes("9");
  if (allSuppressible && Number(digits) === 0) {
    return " ".repeat(expanded.length);
  }

  let index = 0;
  let suppressing = true;
  let out = leadingSign === undefined ? "" : signCharacter(leadingSign, negative);

  for (const symbol of body) {
    if (symbol === "Z" || symbol === "9") {
      const digit = digits[index] as string;
      index += 1;
      if (symbol === "Z" && suppressing && digit === "0") {
        out += " ";
      } else {
        suppressing = false;
        out += digit;
      }
    } else if (symbol === ".") {
      suppressing = false;
      out += ".";
    } else if (symbol === ",") {
      out += suppressing ? " " : ",";
    } else {
      out += symbol;
    }
  }

  return trailingSign === undefined ? out : out + signCharacter(trailingSign, negative);
}

/** `MOVE` into `PIC X(width)`: left justified, space padded, truncated on the right. */
export function alphanumeric(value: string, width: number): string {
  return value.padEnd(width, " ").slice(0, width);
}

/** `MOVE` of a display-numeric item into an alphanumeric one: zero filled digits. */
export function displayDigits(value: number, digits: number): string {
  return String(Math.trunc(Math.abs(value))).padStart(digits, "0").slice(-digits);
}

/**
 * `STRING ... DELIMITED BY <delimiter>`: sends the characters preceding the
 * first occurrence of the delimiter, or the whole item when it does not occur.
 */
export function delimitedBy(value: string, delimiter: string): string {
  const end = value.indexOf(delimiter);
  return end < 0 ? value : value.slice(0, end);
}
