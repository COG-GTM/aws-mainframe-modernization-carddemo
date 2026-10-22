import { loadSeedCatalog } from '../../../src/common/seed/index.js';
import { AccountRecordLayout, TranRecordLayout } from '../../../src/common/copybooks/index.js';
import { Decimal } from '../../../src/common/codecs/decimal.js';
import type { VsamCatalog } from '../../../src/common/vsam/index.js';
import { ONLINE_PROGRAMS, OnlineRuntime, createCardDemoRuntime, type OnlineProgram, type TransactionResult } from '../../../src/online/index.js';
import { NOW, signOn } from '../helpers.js';

export { NOW };
export { errmsg, field, value } from '../helpers.js';

/** No TRANSACT seed exists: the fixtures are the 300 DALYTRAN records (app/data/ASCII/dailytran.txt). */
export function trnRuntime(opts: { clock?: () => Date; replace?: OnlineProgram[]; emptyTransact?: boolean } = {}): { rt: OnlineRuntime; catalog: VsamCatalog } {
  const { catalog } = loadSeedCatalog({ transactFromDailytran: !opts.emptyTransact });
  const clock = opts.clock ?? (() => NOW);
  if (!opts.replace) return { rt: createCardDemoRuntime({ catalog, clock }), catalog };
  const names = new Set(opts.replace.map((p) => p.program));
  const programs = [...ONLINE_PROGRAMS.filter((p) => !names.has(p.program)), ...opts.replace];
  return { rt: new OnlineRuntime({ catalog, clock, programs }), catalog };
}

/** Sign on as USER0001 and pick a COMEN01C option. */
export function menuOption(rt: OnlineRuntime, option: string, term = 'T1'): TransactionResult {
  signOn(rt, 'USER0001', 'PASSWORD', term);
  return rt.sendInput(term, { aid: 'ENTER', fields: { OPTION: option } });
}

export const tranIds = (catalog: VsamCatalog): string[] =>
  catalog.require('TRANSACT').records().map((r) => r.toString('latin1', 0, 16));

export const readTran = (catalog: VsamCatalog, id: string) => {
  const r = catalog.require('TRANSACT').read(id);
  return r ? TranRecordLayout.parse(r) : undefined;
};

export const readAccount = (catalog: VsamCatalog, id: string) => AccountRecordLayout.parse(catalog.require('ACCTDAT').read(id)!);

export function setBalance(catalog: VsamCatalog, id: string, bal: string): void {
  const ds = catalog.require('ACCTDAT');
  const acct = AccountRecordLayout.parse(ds.read(id)!);
  acct.acctCurrBal = new Decimal(bal);
  ds.update(AccountRecordLayout.serialize(acct));
}
