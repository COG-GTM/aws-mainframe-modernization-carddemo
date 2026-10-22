import { loadSeedCatalog, CARDDEMO_CICS_FILES } from '../../common/seed/index.js';
import type { CicsFileDef, VsamCatalog } from '../../common/vsam/index.js';
import { parseAid, type AidKey } from './aid.js';
import { applyInput, emptyScreen, sendText, snapshotScreen, TerminalInputError, type Screen, type ScreenSnapshot } from './bms/screen.js';
import { CicsAbend, CicsCondition, CicsContext, ReturnSignal, XctlSignal, type TaskEnvironment } from './cics.js';
import { TaskFiles } from './files.js';
import { curtimeHhMmSs, curdateMmDdYy } from './helpers.js';
import { ProgramRegistry, type OnlineProgram } from './registry.js';
import { CONDITION_ABEND } from './resp.js';

export interface RuntimeOptions {
  /** VSAM catalog; defaults to `loadSeedCatalog()` (fresh seed data). */
  catalog?: VsamCatalog;
  /** CICS FILE definitions; defaults to `CARDDEMO_CICS_FILES`. */
  files?: Readonly<Record<string, CicsFileDef>>;
  programs?: readonly OnlineProgram[];
  registry?: ProgramRegistry;
  applid?: string;
  sysid?: string;
  /** Task clock (defaults to the wall clock). */
  clock?: () => Date;
  /** FILE names whose updates SYNCPOINT ROLLBACK / abends back out (CSD default: none). */
  recoverableFiles?: readonly string[];
}

/** Pseudo-conversational state of one terminal (replaces the CICS terminal + RETURN TRANSID/COMMAREA). */
export interface TerminalSession {
  readonly id: string;
  screen: Screen;
  /** TRANSID set by the last RETURN TRANSID (started by the next input). */
  nextTransid: string | undefined;
  /** COMMAREA passed on that RETURN. */
  commarea: Buffer | undefined;
  userid: string;
}

export interface TerminalInput {
  aid?: AidKey | string;
  /** Typed values of unprotected named fields. */
  fields?: Readonly<Record<string, string>>;
}

export interface TransactionResult {
  terminalId: string;
  transid: string | null;
  /** Programs that ran in this task, in order (initial program then each XCTL target). */
  programs: string[];
  /** Program that ended the task. */
  program: string | null;
  /** TRANSID that the next input on this terminal will start (null = none). */
  nextTransid: string | null;
  /** COMMAREA kept for the next transaction (latin1 text; null = none). */
  commarea: string | null;
  abend: { code: string; message: string } | null;
  screen: ScreenSnapshot;
}

/**
 * Online runtime: terminals, the transaction/program registry, task dispatch
 * (EIBCALEN/EIBAID), XCTL/LINK/RETURN TRANSID chaining, commit/rollback and abends.
 */
export class OnlineRuntime {
  readonly registry: ProgramRegistry;
  readonly catalog: VsamCatalog;
  readonly files: Readonly<Record<string, CicsFileDef>>;
  readonly applid: string;
  readonly sysid: string;
  readonly tdQueues = new Map<string, Buffer[]>();
  private readonly clock: () => Date;
  private readonly recoverable: ReadonlySet<string>;
  private readonly terminals = new Map<string, TerminalSession>();
  private taskNumber = 0;
  private terminalNumber = 0;

  constructor(opts: RuntimeOptions = {}) {
    this.registry = opts.registry ?? new ProgramRegistry();
    if (opts.programs) this.registry.register(...opts.programs);
    this.catalog = opts.catalog ?? loadSeedCatalog().catalog;
    this.files = opts.files ?? CARDDEMO_CICS_FILES;
    this.applid = (opts.applid ?? 'CARDDEMO').slice(0, 8);
    this.sysid = (opts.sysid ?? 'CDEM').slice(0, 4);
    this.clock = opts.clock ?? ((): Date => new Date());
    this.recoverable = new Set(opts.recoverableFiles ?? []);
  }

  openTerminal(id?: string): TerminalSession {
    const termId = (id ?? `T${String(++this.terminalNumber).padStart(3, '0')}`).slice(0, 4);
    const existing = this.terminals.get(termId);
    if (existing) return existing;
    const t: TerminalSession = { id: termId, screen: emptyScreen(), nextTransid: undefined, commarea: undefined, userid: 'CICSUSER' };
    this.terminals.set(termId, t);
    return t;
  }

  terminal(id: string): TerminalSession | undefined {
    return this.terminals.get(id);
  }

  /** Operator types TRANSID on a cleared screen and presses the AID key (any pending RETURN TRANSID is dropped). */
  startTransaction(terminalId: string, transid: string, input: TerminalInput = {}): TransactionResult {
    const t = this.openTerminal(terminalId);
    t.nextTransid = undefined;
    t.commarea = undefined;
    t.screen = emptyScreen();
    return this.dispatch(t, transid.trim().toUpperCase(), undefined, parseAid(input.aid));
  }

  /** Operator input on the current screen: starts the pending RETURN TRANSID with its COMMAREA. */
  sendInput(terminalId: string, input: TerminalInput = {}): TransactionResult {
    const t = this.terminals.get(terminalId);
    if (!t) throw new TerminalInputError(`terminal ${terminalId} is not open`);
    const aid = parseAid(input.aid);
    if (!t.nextTransid) throw new TerminalInputError('no transaction is pending on this terminal; start one by TRANSID');
    t.screen = applyInput(t.screen, aid, input.fields);
    const transid = t.nextTransid;
    const commarea = t.commarea;
    t.nextTransid = undefined;
    t.commarea = undefined;
    return this.dispatch(t, transid, commarea, aid);
  }

  snapshot(terminalId: string): TransactionResult {
    const t = this.terminals.get(terminalId);
    if (!t) throw new TerminalInputError(`terminal ${terminalId} is not open`);
    return this.result(t, null, [], null, null);
  }

  private result(t: TerminalSession, transid: string | null, programs: string[], program: string | null, abend: TransactionResult['abend']): TransactionResult {
    return {
      terminalId: t.id,
      transid,
      programs,
      program,
      nextTransid: t.nextTransid ?? null,
      commarea: t.commarea ? t.commarea.toString('latin1') : null,
      abend,
      screen: snapshotScreen(t.screen),
    };
  }

  private dispatch(t: TerminalSession, transid: string, commarea: Buffer | undefined, aid: AidKey): TransactionResult {
    const now = this.clock();
    const initial = this.registry.programForTransid(transid);
    if (!initial) {
      t.screen = sendText(t.screen, `DFHAC2001 ${curdateMmDdYy(now)} ${curtimeHhMmSs(now)} ${this.applid} Transaction '${transid}' is not recognized. Check that the transaction name is correct.`);
      return this.result(t, transid, [], null, null);
    }
    const files = new TaskFiles(this.catalog, this.files, this.recoverable);
    const programs: string[] = [];
    const eibBase = {
      eibaid: aid,
      eibtrnid: transid,
      eibdate: (now.getFullYear() - 1900) * 1000 + dayOfYear(now),
      eibtime: now.getHours() * 10000 + now.getMinutes() * 100 + now.getSeconds(),
      eibtaskn: ++this.taskNumber,
      eibtrmid: t.id,
    };
    const env: TaskEnvironment = {
      registry: this.registry,
      files,
      applid: this.applid,
      sysid: this.sysid,
      userid: t.userid,
      termid: t.id,
      now: () => now,
      tdQueues: this.tdQueues,
      screen: t.screen,
      link: (program, area) => {
        const end = runLevel(program, area);
        return end.commarea;
      },
    };
    const runLevel = (start: string, area: Buffer | undefined): { transid: string | undefined; commarea: Buffer | undefined; program: string } => {
      let program = start;
      let ca = area;
      for (;;) {
        const impl = this.registry.program(program);
        if (!impl) throw new CicsAbend('APCT', `program ${program} is not available`);
        programs.push(program);
        const ctx = new CicsContext(env, program, eibBase, ca);
        try {
          invoke(ctx, impl);
          return { transid: undefined, commarea: ctx.commarea, program };
        } catch (e) {
          if (e instanceof XctlSignal) {
            program = e.program;
            ca = e.commarea;
            continue;
          }
          if (e instanceof ReturnSignal) return { transid: e.transid, commarea: e.commarea ?? (e.transid ? undefined : ctx.commarea), program };
          throw e;
        }
      }
    };
    try {
      const end = runLevel(initial, commarea);
      files.syncpoint();
      t.screen = env.screen;
      t.nextTransid = end.transid?.trim();
      t.commarea = end.transid ? end.commarea : undefined;
      return this.result(t, transid, programs, end.program, null);
    } catch (e) {
      files.rollback();
      const abend = toAbend(e);
      const message = `DFHAC2206 ${curtimeHhMmSs(now)} ${this.applid} Transaction ${transid} failed with abend ${abend.code}. Updates to local recoverable resources backed out.`;
      t.screen = sendText(env.screen, message);
      t.nextTransid = undefined;
      t.commarea = undefined;
      return this.result(t, transid, programs, programs.at(-1) ?? null, { code: abend.code, message: abend.detail ?? message });
    }
  }
}

function invoke(ctx: CicsContext, impl: OnlineProgram): void {
  try {
    impl.run(ctx);
  } catch (e) {
    if (e instanceof XctlSignal || e instanceof ReturnSignal) throw e;
    const handler = ctx.abendHandler;
    if (!handler) throw e;
    ctx.abendHandler = undefined;
    const abend = toAbend(e);
    handler(new CicsAbend(abend.code, abend.detail));
  }
}

function toAbend(e: unknown): { code: string; detail?: string } {
  if (e instanceof CicsAbend) return { code: e.code, detail: e.message };
  if (e instanceof CicsCondition) return { code: CONDITION_ABEND[e.resp] ?? 'AEIP', detail: e.message };
  // Program check (the JavaScript equivalent of a data exception / addressing error).
  return { code: 'ASRA', detail: e instanceof Error ? e.message : String(e) };
}

function dayOfYear(d: Date): number {
  return Math.floor((Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()) - Date.UTC(d.getFullYear(), 0, 0)) / 86_400_000);
}
