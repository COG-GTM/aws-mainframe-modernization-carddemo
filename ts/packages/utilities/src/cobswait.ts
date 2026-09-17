/**
 * `COBSWAIT` — wait for the number of centiseconds given as the SYSIN parm.
 *
 * The COBOL program accepts an 8-character SYSIN value, moves it into a
 * `PIC 9(8) COMP` field and calls the assembler routine `MVSWAIT`.
 */

/** Widest value `MVSWAIT-TIME PIC 9(8) COMP` can hold. */
export const MAX_WAIT_CENTISECONDS = 99_999_999;

const CENTISECOND_MS = 10;

export interface WaitResult {
  readonly centiseconds: number;
  readonly milliseconds: number;
  /** `RETURN-CODE`; `0` on success, `12` when the parm is not numeric. */
  readonly returnCode: number;
}

/**
 * Applies the `MOVE PARM-VALUE TO MVSWAIT-TIME` conversion: a right-justified
 * or space-padded numeric string becomes the wait in centiseconds. Blanks are
 * treated as zero, as an all-space SYSIN record is under COBOL.
 */
export function parseWaitParm(parm: string): WaitResult {
  const value = parm.slice(0, 8).trim();
  const centiseconds = value.length === 0 ? 0 : Number(value);

  if (!/^\d*$/.test(value) || centiseconds > MAX_WAIT_CENTISECONDS) {
    return { centiseconds: 0, milliseconds: 0, returnCode: 12 };
  }

  return {
    centiseconds,
    milliseconds: centiseconds * CENTISECOND_MS,
    returnCode: 0,
  };
}

/** The `MVSWAIT` call: suspends the caller for the given centiseconds. */
export async function mvswait(centiseconds: number): Promise<void> {
  await new Promise<void>((resolve) => {
    setTimeout(resolve, centiseconds * CENTISECOND_MS);
  });
}

/**
 * The `COBSWAIT` entry point: `parm` is the SYSIN record. Returns what the
 * program would have waited for and the resulting `RETURN-CODE`.
 */
export async function cobswait(parm: string): Promise<WaitResult> {
  const result = parseWaitParm(parm);
  if (result.returnCode === 0) {
    await mvswait(result.centiseconds);
  }
  return result;
}
