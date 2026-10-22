/**
 * COBOL PICTURE string parser.
 */

export type Usage = 'DISPLAY' | 'COMP' | 'COMP-3' | 'COMP-5' | 'BINARY' | 'PACKED-DECIMAL';

export type PicCategory = 'alphanumeric' | 'numeric' | 'numeric-edited';

export interface Pic {
  /** Original PICTURE string, e.g. `S9(10)V99`. */
  readonly source: string;
  /** PICTURE with repetition factors expanded, e.g. `S999999999999V99` -> `S9999999999V99`. */
  readonly expanded: string;
  readonly category: PicCategory;
  /** Total number of digit positions (numeric) or characters (alphanumeric / edited). */
  readonly digits: number;
  /** Number of digit positions to the right of the implied/actual decimal point. */
  readonly scale: number;
  readonly signed: boolean;
  /** Display length in characters (DISPLAY usage). */
  readonly displayLength: number;
}

/** Expands repetition factors: `X(3)9(2)` -> `XXX99`. */
export function expandPic(pic: string): string {
  return pic
    .toUpperCase()
    .replace(/\s+/g, '')
    .replace(/(.)\((\d+)\)/g, (_m, ch: string, n: string) => ch.repeat(Number(n)));
}

export function parsePic(source: string): Pic {
  const expanded = expandPic(source);
  if (/^[XA]+$/.test(expanded)) {
    return {
      source,
      expanded,
      category: 'alphanumeric',
      digits: expanded.length,
      scale: 0,
      signed: false,
      displayLength: expanded.length,
    };
  }
  const numeric = /^(S?)(9*)(?:V(9*))?$/.exec(expanded);
  if (numeric && (numeric[2]!.length > 0 || (numeric[3] ?? '').length > 0)) {
    const intDigits = numeric[2]!.length;
    const scale = (numeric[3] ?? '').length;
    if (intDigits + scale > 31) throw new Error(`PIC ${source}: more than 31 digits`);
    return {
      source,
      expanded,
      category: 'numeric',
      digits: intDigits + scale,
      scale,
      signed: numeric[1] === 'S',
      displayLength: intDigits + scale,
    };
  }
  if (/^[9Z*.,B0/+\-$VCRDB]+$/.test(expanded) && /[Z*.,B0/+\-$]|CR|DB/.test(expanded)) {
    const body = expanded.replace(/V/g, '');
    const pointAt = body.indexOf('.');
    const isDigitPos = (s: string, i: number): boolean => '9Z*'.includes(s[i]!);
    let digits = 0;
    let scale = 0;
    for (let i = 0; i < body.length; i++) {
      if (isDigitPos(body, i)) {
        digits++;
        if (pointAt >= 0 && i > pointAt) scale++;
      }
    }
    return {
      source,
      expanded,
      category: 'numeric-edited',
      digits,
      scale,
      signed: /[+-]|CR|DB/.test(body),
      displayLength: body.length,
    };
  }
  throw new Error(`Unsupported PICTURE string: ${source}`);
}

/** Storage length in bytes for a picture with a given usage. */
export function storageLength(pic: Pic, usage: Usage): number {
  switch (usage) {
    case 'DISPLAY':
      return pic.displayLength;
    case 'COMP-3':
    case 'PACKED-DECIMAL':
      assertNumeric(pic, usage);
      return Math.floor(pic.digits / 2) + 1;
    case 'COMP':
    case 'COMP-5':
    case 'BINARY':
      assertNumeric(pic, usage);
      return binaryLength(pic.digits);
  }
}

/** IBM/GnuCOBOL (binary-size 2-4-8) storage size of a binary item. */
export function binaryLength(digits: number): number {
  if (digits <= 4) return 2;
  if (digits <= 9) return 4;
  if (digits <= 18) return 8;
  throw new Error(`Binary item with ${digits} digits is not supported`);
}

function assertNumeric(pic: Pic, usage: Usage): void {
  if (pic.category !== 'numeric') throw new Error(`USAGE ${usage} requires a numeric PIC, got ${pic.source}`);
}
