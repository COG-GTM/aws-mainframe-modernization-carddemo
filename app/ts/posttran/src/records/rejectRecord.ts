import { assembleRecord, padRecord, readDigits, readText, writeDigits, writeText } from '../codec/fixedWidth.ts';

/**
 * `REJECT-RECORD` — declared in CBTRN02C itself (lines 176-182), 430 bytes:
 * the untouched 350-byte daily transaction image plus the 80-byte validation
 * trailer (`9(04)` reason code + `X(76)` description).
 */
export interface RejectRecord {
  readonly transactionImage: string;
  readonly reasonCode: number;
  readonly reasonDescription: string;
}

export const REJECT_LENGTH = 430;

export function parseRejectRecord(line: string): RejectRecord {
  const image = padRecord(line, REJECT_LENGTH);
  return {
    transactionImage: readText(image, 1, 350),
    reasonCode: Number(readDigits(image, 351, 4)),
    reasonDescription: readText(image, 355, 76),
  };
}

export function serializeRejectRecord(record: RejectRecord): string {
  return assembleRecord(
    [
      writeText(record.transactionImage, 350),
      writeDigits(String(record.reasonCode), 4),
      writeText(record.reasonDescription, 76),
    ],
    REJECT_LENGTH,
  );
}
