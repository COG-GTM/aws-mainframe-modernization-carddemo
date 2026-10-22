/** COBOL FILE STATUS codes and CICS RESP conditions used by the file layers. */

export const FileStatus = {
  /** Successful completion. */
  SUCCESS: '00',
  /** Success; a duplicate alternate key exists (READ: more follow; WRITE/REWRITE: duplicate created). */
  DUPLICATE_ALT_KEY: '02',
  /** Success; record length does not match the fixed file attribute. */
  LENGTH_MISMATCH: '04',
  /** OPEN of an OPTIONAL file that is not present. */
  OPTIONAL_NOT_PRESENT: '05',
  /** End of file (sequential READ). */
  END_OF_FILE: '10',
  /** Sequence error (sequential WRITE out of key order, or REWRITE changed the prime key). */
  SEQUENCE_ERROR: '21',
  /** Duplicate prime key or unique alternate key on WRITE/REWRITE. */
  DUPLICATE_KEY: '22',
  /** Record not found (random READ, START, DELETE, REWRITE). */
  NOT_FOUND: '23',
  /** Permanent I/O error. */
  PERMANENT_ERROR: '30',
  /** OPEN INPUT / I-O / EXTEND of a non-optional file that does not exist. */
  FILE_NOT_FOUND: '35',
  /** OPEN mode not supported by the file. */
  OPEN_MODE_NOT_ALLOWED: '37',
  /** OPEN of a file that is already open. */
  ALREADY_OPEN: '41',
  /** CLOSE of a file that is not open. */
  NOT_OPEN: '42',
  /** REWRITE/DELETE in sequential access not preceded by a successful READ. */
  NO_PRIOR_READ: '43',
  /** Sequential READ with no valid next record (after EOF or a failed READ/START). */
  NO_NEXT_RECORD: '46',
  /** READ/START on a file not open INPUT or I-O. */
  READ_NOT_OPEN_INPUT: '47',
  /** WRITE on a file not open OUTPUT, I-O or EXTEND. */
  WRITE_NOT_OPEN_OUTPUT: '48',
  /** REWRITE/DELETE on a file not open I-O. */
  REWRITE_NOT_OPEN_IO: '49',
} as const;
export type FileStatusCode = (typeof FileStatus)[keyof typeof FileStatus];

/** True for the "successful" status class (first digit 0). */
export const isSuccessful = (status: string): boolean => status.startsWith('0');

/** CICS RESP condition values (DFHRESP). */
export const Resp = {
  NORMAL: 0,
  ERROR: 1,
  FILENOTFOUND: 12,
  NOTFND: 13,
  DUPREC: 14,
  DUPKEY: 15,
  INVREQ: 16,
  IOERR: 17,
  NOSPACE: 18,
  NOTOPEN: 19,
  ENDFILE: 20,
  ILLOGIC: 21,
  LENGERR: 22,
  DISABLED: 84,
} as const;
export type RespCode = (typeof Resp)[keyof typeof Resp];

/** Name of a RESP value (e.g. 13 -> 'NOTFND'). */
export function respName(resp: number): string {
  for (const [name, value] of Object.entries(Resp)) if (value === resp) return name;
  return String(resp);
}
