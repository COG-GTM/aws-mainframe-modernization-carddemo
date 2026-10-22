import type { CicsContext } from './cics.js';

/** An online (CICS) program port. `run` is the PROCEDURE DIVISION entry. */
export interface OnlineProgram {
  /** PROGRAM name as defined in the CSD / used on XCTL and LINK (e.g. 'COSGN00C'). */
  readonly program: string;
  /** TRANSID whose initial program this is (e.g. 'CC00'); defaults to the CSD table. */
  readonly transid?: string;
  run(ctx: CicsContext): void;
}

/** DEFINE TRANSACTION(...) PROGRAM(...) entries of app/csd/CARDDEMO.CSD. */
export const CSD_TRANSACTIONS: Readonly<Record<string, string>> = Object.freeze({
  CAUP: 'COACTUPC',
  CAVW: 'COACTVWC',
  CA00: 'COADM01C',
  CB00: 'COBIL00C',
  CCDL: 'COCRDSLC',
  CCLI: 'COCRDLIC',
  CCUP: 'COCRDUPC',
  CC00: 'COSGN00C',
  CDV1: 'COCRDSEC',
  CM00: 'COMEN01C',
  CR00: 'CORPT00C',
  CT00: 'COTRN00C',
  CT01: 'COTRN01C',
  CT02: 'COTRN02C',
  CU00: 'COUSR00C',
  CU01: 'COUSR01C',
  CU02: 'COUSR02C',
  CU03: 'COUSR03C',
});

/**
 * Program/transaction registry. A program that is not registered behaves like
 * a CICS program with no installed load module: XCTL/LINK raise PGMIDERR,
 * INQUIRE PROGRAM returns PGMIDERR, and starting its transaction abends.
 */
export class ProgramRegistry {
  private readonly programs = new Map<string, OnlineProgram>();
  private readonly transactions = new Map<string, string>(Object.entries(CSD_TRANSACTIONS));

  constructor(programs: readonly OnlineProgram[] = []) {
    this.register(...programs);
  }

  register(...programs: OnlineProgram[]): this {
    for (const p of programs) {
      const name = p.program.trim();
      if (this.programs.has(name)) throw new Error(`online program ${name} registered twice`);
      this.programs.set(name, p);
      if (p.transid) this.transactions.set(p.transid.trim(), name);
    }
    return this;
  }

  program(name: string): OnlineProgram | undefined {
    return this.programs.get(name.trim());
  }

  isInstalled(name: string): boolean {
    return this.programs.has(name.trim());
  }

  /** Initial program of a TRANSID, or undefined if the transaction is not defined. */
  programForTransid(transid: string): string | undefined {
    return this.transactions.get(transid.trim().toUpperCase());
  }

  programNames(): string[] {
    return [...this.programs.keys()].sort();
  }

  transactionTable(): Record<string, { program: string; installed: boolean }> {
    const out: Record<string, { program: string; installed: boolean }> = {};
    for (const [t, p] of [...this.transactions].sort()) out[t] = { program: p, installed: this.programs.has(p) };
    return out;
  }
}
