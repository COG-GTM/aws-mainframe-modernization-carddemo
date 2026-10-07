/** Decimal -> `+ZZZ,ZZZ,ZZZ.99` style used by the account screens. */
export function money(value: string | number | undefined | null): string {
  if (value === undefined || value === null || value === '') return '';
  const n = Number(value);
  if (Number.isNaN(n)) return String(value);
  const abs = Math.abs(n).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  return `${n < 0 ? '-' : '+'}${abs}`;
}

export const text = (v: unknown): string => (v === null || v === undefined ? '' : String(v));
