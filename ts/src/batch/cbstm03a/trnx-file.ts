/**
 * TRNXFILE for CBSTM03A, built the way the CREASTMT job builds it:
 *
 * - STEP010 SORT: `SORT FIELDS=(263,16,CH,A,1,16,CH,A)` over TRANSACT
 *   (card number, then transaction id), `OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)`
 *   i.e. card number moved to the front (COSTM01 TRNX-RECORD); the 328-byte
 *   reformatted record is padded to LRECL 350 with spaces.
 * - STEP020 IDCAMS REPRO into AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS (KEYS(32 0), RECORDSIZE(350 350)).
 *
 * There is no TRANSACT seed in app/data: use `loadSeedCatalog({ transactFromDailytran: true })`
 * (TRANSACT loaded from dailytran.txt) or a TRANSACT written by POSTTRAN.
 */
import { asciiToEbcdic } from '../../common/codecs/ebcdic.js';
import type { VsamCatalog } from '../../common/vsam/catalog.js';
import { IndexedFile } from '../../common/vsam/cobol-file.js';
import type { VsamDataset } from '../../common/vsam/dataset.js';

export const TRXFL_DSN = 'AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS';
export const TRXFL_RECORD_LENGTH = 350;

/** SORT key CH,A compares EBCDIC bytes (the data is held in ASCII). */
function sortKey(rec: Buffer): string {
  const key = Buffer.concat([rec.subarray(262, 278), rec.subarray(0, 16)]);
  return asciiToEbcdic(key).toString('latin1');
}

/** CREASTMT STEP010: sort + reformat 350-byte TRANSACT records into TRNX records. */
export function sortTransactToTrnx(transact: readonly Buffer[]): Buffer[] {
  const keyed = transact.map((rec, i) => {
    if (rec.length !== TRXFL_RECORD_LENGTH) throw new RangeError(`TRANSACT record ${i + 1} is ${rec.length} bytes, expected 350`);
    return { rec, key: sortKey(rec), i };
  });
  // DFSORT with EQUALS-like tie break on input order (keys are unique in practice).
  keyed.sort((a, b) => (a.key < b.key ? -1 : a.key > b.key ? 1 : a.i - b.i));
  return keyed.map(({ rec }) => {
    const out = Buffer.alloc(TRXFL_RECORD_LENGTH, 0x20);
    rec.copy(out, 0, 262, 278); // 1:263,16
    rec.copy(out, 16, 0, 262); // 17:1,262
    rec.copy(out, 278, 278, 328); // 279:279,50
    return out;
  });
}

/** Reads TRANSACT sequentially in prime-key order (SORTIN of STEP010). */
export function readTransact(catalog: VsamCatalog, name = 'TRANSACT'): Buffer[] {
  const f = new IndexedFile(catalog, name, { access: 'SEQUENTIAL' });
  const opened = f.open('INPUT');
  if (opened.status !== '00') throw new Error(`OPEN ${name}: status ${opened.status}`);
  const out: Buffer[] = [];
  for (let r = f.readNext(); r.status === '00' || r.status === '02'; r = f.readNext()) out.push(r.record!);
  f.close();
  return out;
}

export interface TrnxBuildResult {
  dataset: VsamDataset;
  loaded: number;
  /** Records REPRO could not load (duplicate 32-byte key). */
  rejected: number;
}

/**
 * CREASTMT DELDEF01 + STEP010 + STEP020: (re)defines TRXFL (alias TRNXFILE) in
 * the catalog and loads it from `transact` (default: the catalog's TRANSACT cluster).
 */
export function buildTrnxFile(catalog: VsamCatalog, transact?: readonly Buffer[]): TrnxBuildResult {
  const input = transact ?? readTransact(catalog);
  catalog.delete(TRXFL_DSN);
  const dataset = catalog.define(
    { name: TRXFL_DSN, organization: 'KSDS', recordLength: TRXFL_RECORD_LENGTH, key: { offset: 0, length: 32 } },
    ['TRNXFILE'],
  );
  const res = dataset.load(sortTransactToTrnx(input));
  return { dataset, loaded: res.loaded, rejected: res.rejected.length };
}
