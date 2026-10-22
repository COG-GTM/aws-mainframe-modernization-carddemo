/**
 * Pieces shared by CBEXPORT and CBIMPORT: the export KSDS definition from
 * app/jcl/CBEXPORT.jcl, the DISPLAY/clock formatting both programs use, and
 * the CEE3ABD abend signal.
 */
import { ExportRecordLayout } from '../../common/copybooks/export.js';
import type { VsamCatalog } from '../../common/vsam/catalog.js';
import type { DatasetDef, VsamDataset } from '../../common/vsam/dataset.js';

/**
 * DEFINE CLUSTER from CBEXPORT.jcl: KEYS(4 28) RECORDSIZE(500 500).
 * IDCAMS offsets are 0-based, so the prime key is bytes 28-31: the low three
 * bytes of EXPORT-SEQUENCE-NUM (offset 27, 4-byte COMP) plus the first byte of
 * EXPORT-BRANCH-ID, not the field the programs name as RECORD KEY. Kept as
 * defined; key order equals sequence order while the counter is below 2**24.
 * Collation is raw bytes because the key holds binary data.
 */
export const EXPORT_CLUSTER_DEF: DatasetDef = {
  name: 'AWS.M2.CARDDEMO.EXPORT.DATA',
  organization: 'KSDS',
  recordLength: ExportRecordLayout.length,
  key: { offset: 28, length: 4 },
  collation: 'ascii',
};

/** DD name both programs use for the export file. */
export const EXPORT_DD = 'EXPFILE';

/** Defines (or redefines, like the JCL DELETE/DEFINE step) the empty export cluster. */
export function defineExportCluster(catalog: VsamCatalog): VsamDataset {
  if (catalog.has(EXPORT_CLUSTER_DEF.name)) catalog.delete(EXPORT_CLUSTER_DEF.name);
  if (catalog.has(EXPORT_DD)) catalog.delete(EXPORT_DD);
  return catalog.define(EXPORT_CLUSTER_DEF, [EXPORT_DD]);
}

/** DISPLAY of an unsigned PIC 9(n) DISPLAY counter. */
export function pic9(value: number, digits = 9): string {
  return String(value % 10 ** digits).padStart(digits, '0');
}

const p2 = (n: number) => String(n).padStart(2, '0');

/** ACCEPT ... FROM DATE YYYYMMDD / FROM TIME (HHMMSShh), local time. */
export function acceptDateTime(now: Date): { date: string; time: string } {
  return {
    date: `${now.getFullYear()}${p2(now.getMonth() + 1)}${p2(now.getDate())}`,
    time: `${p2(now.getHours())}${p2(now.getMinutes())}${p2(now.getSeconds())}${p2(Math.floor(now.getMilliseconds() / 10))}`,
  };
}

/** FUNCTION CURRENT-DATE: YYYYMMDDHHMMSShh+hhmm (21 characters), local time. */
export function currentDate(now: Date): string {
  const { date, time } = acceptDateTime(now);
  const off = -now.getTimezoneOffset();
  const sign = off < 0 ? '-' : '+';
  const abs = Math.abs(off);
  return `${date}${time}${sign}${p2(Math.floor(abs / 60))}${p2(abs % 60)}`;
}

/** Thrown by 9999-ABEND-PROGRAM (CALL 'CEE3ABD') to stop the run. */
export class ProgramAbend extends Error {
  constructor(readonly program: string) {
    super(`${program}: ABENDING PROGRAM`);
  }
}
