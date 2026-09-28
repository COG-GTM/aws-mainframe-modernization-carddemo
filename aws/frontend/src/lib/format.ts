const pad = (n: number, len = 2) => String(n).padStart(len, '0');

export function legacyDate(d: Date): string {
  return `${pad(d.getMonth() + 1)}/${pad(d.getDate())}/${pad(d.getFullYear() % 100)}`;
}

export function legacyTime(d: Date): string {
  return `${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

export function isoToday(d: Date = new Date()): string {
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}`;
}

/** `2022-06-10 19:27:53.000000` or `2022-06-10` -> `06/10/22` (COTRN00 TDATE, PIC X(08)). */
export function shortDate(value: string): string {
  const m = /^(\d{4})-(\d{2})-(\d{2})/.exec(value);
  return m ? `${m[2]}/${m[3]}/${m[1].slice(2)}` : value;
}

export function datePart(value: string): string {
  return value.slice(0, 10);
}

/** Decimal string -> `+ZZZ,ZZZ,ZZZ.99` style display used by the account screens. */
export function money(value: string | undefined | null): string {
  if (value === undefined || value === null || value === '') return '';
  const n = Number(value);
  if (Number.isNaN(n)) return value;
  const abs = Math.abs(n).toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  return `${n < 0 ? '-' : '+'}${abs}`;
}

export function formatSsn(ssn: string): string {
  const d = ssn.replace(/\D/g, '');
  return d.length === 9 ? `${d.slice(0, 3)}-${d.slice(3, 5)}-${d.slice(5)}` : ssn;
}

export function padKey(value: string, len: number): string {
  return value.trim().padStart(len, '0');
}
