/**
 * Port of app/cbl/COBSWAIT.cbl: ACCEPT PARM-VALUE (X(8)) FROM SYSIN,
 * MOVE it to MVSWAIT-TIME (9(8) COMP) and CALL 'MVSWAIT' (wait in centiseconds).
 */

/**
 * MOVE PIC X(8) -> PIC 9(8): the alphanumeric sender is treated as an unsigned
 * display number, each byte contributing its low-order nibble (so '0'-'9' give
 * their digit and a space gives 0, in both ASCII and EBCDIC).
 */
export function parseWaitParm(sysin: string): number {
  const parm = sysin.padEnd(8, ' ').slice(0, 8);
  let value = 0;
  for (const ch of parm) value = value * 10 + ((ch.charCodeAt(0) & 0x0f) % 10);
  return value;
}

export type Sleeper = (ms: number) => Promise<void>;
const defaultSleep: Sleeper = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/** Runs COBSWAIT: returns the centiseconds waited. */
export async function cobswait(sysin: string, sleep: Sleeper = defaultSleep): Promise<number> {
  const centiseconds = parseWaitParm(sysin);
  await sleep(centiseconds * 10);
  return centiseconds;
}
