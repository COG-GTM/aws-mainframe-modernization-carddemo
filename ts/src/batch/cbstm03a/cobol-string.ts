/**
 * COBOL `STRING ... DELIMITED BY ... INTO target` (no POINTER), as used by CBSTM03A.
 *
 * Each source contributes its characters up to (not including) the first
 * occurrence of its delimiter, or all of them for `DELIMITED BY SIZE`. Transfer
 * stops when the target is full (overflow). Target positions that are not
 * written keep their previous content - STRING never pads.
 */
export interface StringSource {
  value: string;
  /** Delimiter literal, or 'SIZE'. */
  delimitedBy: string | { size: true };
}

export const SIZE = { size: true } as const;

export function cobolString(target: string, sources: readonly StringSource[]): { result: string; overflow: boolean } {
  const out = target.split('');
  let ptr = 0;
  for (const src of sources) {
    let part = src.value;
    if (typeof src.delimitedBy === 'string') {
      const at = part.indexOf(src.delimitedBy);
      if (at >= 0) part = part.slice(0, at);
    }
    for (const ch of part) {
      if (ptr >= out.length) return { result: out.join(''), overflow: true };
      out[ptr++] = ch;
    }
  }
  return { result: out.join(''), overflow: false };
}

/** `MOVE SPACES TO target` then STRING into it. */
export function stringInto(length: number, sources: readonly StringSource[]): string {
  return cobolString(' '.repeat(length), sources).result;
}
