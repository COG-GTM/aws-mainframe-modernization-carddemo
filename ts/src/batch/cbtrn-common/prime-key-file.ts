import { FileStatus, IndexedFile, type IoResult } from '../../common/vsam/index.js';

/**
 * An INDEXED file whose SELECT declares no ALTERNATE RECORD KEY. VSAM still
 * maintains the cluster's upgrade-set AIXs (e.g. TRANAIX on TRANSACT), but the
 * program never sees status 02 for them: duplicate alternate keys report 00.
 * (IndexedFile reports 02 for every AIX of the cluster.)
 */
export class PrimeKeyOnlyIndexedFile extends IndexedFile {
  private normalize(res: IoResult): IoResult {
    if (res.status !== FileStatus.DUPLICATE_ALT_KEY) return res;
    this.status = FileStatus.SUCCESS;
    return { ...res, status: FileStatus.SUCCESS };
  }

  override read(keyValue: string | Uint8Array, keyName?: string): IoResult {
    return this.normalize(super.read(keyValue, keyName));
  }

  override readNext(): IoResult {
    return this.normalize(super.readNext());
  }

  override write(record: Uint8Array): IoResult {
    return this.normalize(super.write(record));
  }

  override rewrite(record: Uint8Array): IoResult {
    return this.normalize(super.rewrite(record));
  }
}
