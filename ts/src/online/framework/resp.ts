import { Resp } from '../../common/vsam/index.js';

/** DFHRESP values: the VSAM/file ones from the foundation plus the online-only conditions. */
export const DFHRESP = {
  ...Resp,
  PGMIDERR: 27,
  TRANSIDERR: 28,
  MAPFAIL: 36,
  INVMPSZ: 38,
  QIDERR: 44,
  ROLLEDBACK: 82,
} as const;

/** CICS abend codes raised for unhandled conditions (EXEC interface AEIx/AEYx family). */
export const CONDITION_ABEND: Readonly<Record<number, string>> = {
  [DFHRESP.ERROR]: 'AEIP',
  [DFHRESP.FILENOTFOUND]: 'AEIL',
  [DFHRESP.NOTFND]: 'AEIM',
  [DFHRESP.DUPREC]: 'AEIN',
  [DFHRESP.DUPKEY]: 'AEIO',
  [DFHRESP.INVREQ]: 'AEIP',
  [DFHRESP.IOERR]: 'AEIQ',
  [DFHRESP.NOSPACE]: 'AEIR',
  [DFHRESP.NOTOPEN]: 'AEIS',
  [DFHRESP.ENDFILE]: 'AEIT',
  [DFHRESP.ILLOGIC]: 'AEIU',
  [DFHRESP.LENGERR]: 'AEIV',
  [DFHRESP.PGMIDERR]: 'AEI0',
  [DFHRESP.TRANSIDERR]: 'AEI1',
  [DFHRESP.MAPFAIL]: 'AEI9',
  [DFHRESP.QIDERR]: 'AEYH',
  [DFHRESP.DISABLED]: 'AEXL',
};

export const respNameOf = (resp: number): string =>
  Object.entries(DFHRESP).find(([, v]) => v === resp)?.[0] ?? `RESP${resp}`;
