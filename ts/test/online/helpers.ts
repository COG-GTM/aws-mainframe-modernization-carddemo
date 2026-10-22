import { loadSeedCatalog } from '../../src/common/seed/index.js';
import { createCardDemoRuntime, type OnlineProgram, type TransactionResult } from '../../src/online/index.js';

export const NOW = new Date(2026, 9, 22, 14, 5, 9);

export function runtime(extra: OnlineProgram[] = []) {
  const rt = createCardDemoRuntime({ catalog: loadSeedCatalog().catalog, clock: () => NOW });
  if (extra.length) rt.registry.register(...extra);
  return rt;
}

export const field = (r: TransactionResult, name: string) => {
  const f = r.screen.fields.find((x) => x.name === name);
  if (!f) throw new Error(`no field ${name} on ${r.screen.map}`);
  return f;
};
export const value = (r: TransactionResult, name: string): string => field(r, name).value.trimEnd();
export const errmsg = (r: TransactionResult): string => value(r, 'ERRMSG');

/** Stub program recording the commarea it was XCTLed with. */
export function stub(program: string, seen: Buffer[] = []): OnlineProgram {
  return {
    program,
    run(ctx) {
      seen.push(ctx.commarea ?? Buffer.alloc(0));
      ctx.sendText(`${program} reached`);
      ctx.return();
    },
  };
}

export function signOn(rt: ReturnType<typeof runtime>, user: string, pwd: string, term = 'T1'): TransactionResult {
  rt.openTerminal(term);
  rt.startTransaction(term, 'CC00');
  return rt.sendInput(term, { aid: 'ENTER', fields: { USERID: user, PASSWD: pwd } });
}
