/**
 * Equivalent of `9999-ABEND-PROGRAM` (CBTRN02C l.707-711): an unrecoverable
 * I/O condition. COBOL abends with user code 999 after displaying the file
 * status; here the condition is thrown and reported by the entrypoint.
 */
export class FileAbendError extends Error {
  readonly ddName: string;
  readonly fileStatus: string;

  constructor(message: string, ddName: string, fileStatus: string) {
    super(`${message} (DD ${ddName}, FILE STATUS IS: NNNN00${fileStatus})`);
    this.name = 'FileAbendError';
    this.ddName = ddName;
    this.fileStatus = fileStatus;
  }
}

/** VSAM file statuses used by this job. */
export const FILE_STATUS = {
  ok: '00',
  duplicateKey: '22',
  recordNotFound: '23',
} as const;
