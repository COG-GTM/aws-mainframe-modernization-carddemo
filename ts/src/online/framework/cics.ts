import type { AidKey } from './aid.js';
import { receiveMap as bmsReceive, sendMap as bmsSend, sendText as bmsSendText, type Screen, type SendMapOptions } from './bms/screen.js';
import type { SymbolicMap } from './bms/map.js';
import type { TaskFiles } from './files.js';
import type { ProgramRegistry } from './registry.js';
import { DFHRESP } from './resp.js';

/** EXEC Interface Block fields used by CardDemo. */
export interface Eib {
  eibcalen: number;
  eibaid: AidKey;
  eibtrnid: string;
  eibresp: number;
  eibresp2: number;
  /** 0CYYDDD */
  eibdate: number;
  /** HHMMSS */
  eibtime: number;
  eibtaskn: number;
  eibtrmid: string;
}

export interface CommandResult {
  resp: number;
  resp2: number;
}

/** Control transfer signals (thrown; never caught by program code). */
export class XctlSignal {
  constructor(
    readonly program: string,
    readonly commarea: Buffer | undefined,
  ) {}
}
export class ReturnSignal {
  constructor(
    readonly transid: string | undefined,
    readonly commarea: Buffer | undefined,
  ) {}
}

/** Task abend (EXEC CICS ABEND, unhandled condition, or program check). */
export class CicsAbend extends Error {
  constructor(
    readonly code: string,
    message?: string,
  ) {
    super(message ?? `abend ${code}`);
  }
}

/** Unhandled CICS condition (no RESP/NOHANDLE): becomes an abend unless the program catches it. */
export class CicsCondition extends Error {
  constructor(
    readonly command: string,
    readonly resp: number,
    readonly resp2 = 0,
  ) {
    super(`${command} raised RESP ${resp}`);
  }
}

export const isControlSignal = (e: unknown): e is XctlSignal | ReturnSignal => e instanceof XctlSignal || e instanceof ReturnSignal;

/** Per-task services supplied by the runtime. */
export interface TaskEnvironment {
  readonly registry: ProgramRegistry;
  readonly files: TaskFiles;
  readonly applid: string;
  readonly sysid: string;
  readonly userid: string;
  readonly termid: string;
  readonly now: () => Date;
  readonly tdQueues: Map<string, Buffer[]>;
  screen: Screen;
  /** Runs a LINKed program in this task; returns the (possibly updated) commarea. */
  link(program: string, commarea: Buffer | undefined): Buffer | undefined;
}

export interface AssignResult {
  applid: string;
  sysid: string;
  userid: string;
  termid: string;
  netname: string;
}

/**
 * EXEC CICS command surface for one program invocation (one link level).
 * Commands that take RESP in CardDemo return `{ resp, resp2 }` and also set
 * `eib.eibresp`; XCTL/RETURN throw control signals.
 */
export class CicsContext {
  readonly eib: Eib;
  /** DFHCOMMAREA (a private copy the program may modify, e.g. for LINK). */
  readonly commarea: Buffer | undefined;
  /** HANDLE ABEND LABEL(...) equivalent: invoked once if the program abends. */
  abendHandler: ((abend: CicsAbend) => void) | undefined;

  constructor(
    private readonly env: TaskEnvironment,
    readonly program: string,
    eib: Omit<Eib, 'eibcalen' | 'eibresp' | 'eibresp2'>,
    commarea: Buffer | undefined,
  ) {
    this.commarea = commarea ? Buffer.from(commarea) : undefined;
    this.eib = { ...eib, eibcalen: this.commarea?.length ?? 0, eibresp: 0, eibresp2: 0 };
  }

  get files(): TaskFiles {
    return this.env.files;
  }

  /** Current task time (FUNCTION CURRENT-DATE / ASKTIME source). */
  now(): Date {
    return this.env.now();
  }

  private result(resp: number, resp2 = 0): CommandResult {
    this.eib.eibresp = resp;
    this.eib.eibresp2 = resp2;
    return { resp, resp2 };
  }

  /** Records RESP/RESP2 of a file command in the EIB and passes the result through. */
  track<T extends CommandResult>(r: T): T {
    this.result(r.resp, r.resp2);
    return r;
  }

  sendMap(from: SymbolicMap, opts: SendMapOptions & { mapset?: string } = {}): CommandResult {
    if (opts.mapset !== undefined && opts.mapset.trim() !== from.def.mapset) throw new CicsAbend('APCT', `mapset ${opts.mapset} does not contain ${from.name}`);
    this.env.screen = bmsSend(this.env.screen, from, opts);
    return this.result(DFHRESP.NORMAL);
  }

  receiveMap(into: SymbolicMap): CommandResult {
    const resp = bmsReceive(this.env.screen, into, this.eib.eibaid);
    return this.result(resp);
  }

  /** SEND TEXT FROM(text) [ERASE] [FREEKB]. */
  sendText(text: string): CommandResult {
    this.env.screen = bmsSendText(this.env.screen, text);
    return this.result(DFHRESP.NORMAL);
  }

  /** EXEC CICS RETURN [TRANSID(...)] [COMMAREA(...)]. */
  return(opts: { transid?: string; commarea?: Buffer } = {}): never {
    throw new ReturnSignal(opts.transid, opts.commarea ? Buffer.from(opts.commarea) : undefined);
  }

  /**
   * EXEC CICS XCTL PROGRAM(...) [COMMAREA(...)]. Does not return on success.
   * An unavailable program raises PGMIDERR as a `CicsCondition` (catch it to
   * model HANDLE CONDITION PGMIDERR), or, with `{ resp: true }`, returns it.
   */
  xctl(program: string, commarea?: Buffer, opts: { resp?: boolean } = {}): CommandResult {
    if (!this.env.registry.isInstalled(program)) {
      if (opts.resp) return this.result(DFHRESP.PGMIDERR, 3);
      this.result(DFHRESP.PGMIDERR, 3);
      throw new CicsCondition(`XCTL PROGRAM(${program.trim()})`, DFHRESP.PGMIDERR, 3);
    }
    throw new XctlSignal(program.trim(), commarea ? Buffer.from(commarea) : undefined);
  }

  /** EXEC CICS LINK PROGRAM(...) COMMAREA(...): returns the callee's commarea. */
  link(program: string, commarea?: Buffer, opts: { resp?: boolean } = {}): CommandResult & { commarea?: Buffer } {
    if (!this.env.registry.isInstalled(program)) {
      if (opts.resp) return this.result(DFHRESP.PGMIDERR, 3);
      this.result(DFHRESP.PGMIDERR, 3);
      throw new CicsCondition(`LINK PROGRAM(${program.trim()})`, DFHRESP.PGMIDERR, 3);
    }
    const out = this.env.link(program.trim(), commarea);
    const r: CommandResult & { commarea?: Buffer } = this.result(DFHRESP.NORMAL);
    if (out) r.commarea = out;
    return r;
  }

  /** EXEC CICS INQUIRE PROGRAM(...) NOHANDLE. */
  inquireProgram(program: string): CommandResult {
    return this.env.registry.isInstalled(program) ? this.result(DFHRESP.NORMAL) : this.result(DFHRESP.PGMIDERR, 1);
  }

  /** EXEC CICS ASSIGN APPLID/SYSID/USERID/... */
  assign(): AssignResult {
    this.result(DFHRESP.NORMAL);
    return { applid: this.env.applid, sysid: this.env.sysid, userid: this.env.userid, termid: this.env.termid, netname: this.env.termid };
  }

  /** EXEC CICS ASKTIME ABSTIME(...): the task clock. */
  asktime(): Date {
    this.result(DFHRESP.NORMAL);
    return this.env.now();
  }

  /** EXEC CICS WRITEQ TD QUEUE(...) FROM(...). */
  writeqTd(queue: string, from: string | Uint8Array): CommandResult {
    const q = queue.trim();
    const list = this.env.tdQueues.get(q) ?? [];
    list.push(Buffer.from(typeof from === 'string' ? Buffer.from(from, 'latin1') : from));
    this.env.tdQueues.set(q, list);
    return this.result(DFHRESP.NORMAL);
  }

  /** EXEC CICS SYNCPOINT. */
  syncpoint(): CommandResult {
    this.env.files.syncpoint();
    return this.result(DFHRESP.NORMAL);
  }

  /** EXEC CICS SYNCPOINT ROLLBACK. */
  syncpointRollback(): CommandResult {
    this.env.files.rollback();
    return this.result(DFHRESP.NORMAL);
  }

  /** EXEC CICS ABEND ABCODE(...). */
  abend(code: string, message?: string): never {
    throw new CicsAbend(code.trim(), message);
  }
}
