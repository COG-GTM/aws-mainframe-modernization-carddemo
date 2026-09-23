import {
  assembleRecord,
  padRecord,
  readDigits,
  readText,
  writeDigits,
  writeText,
} from '../codec/fixedWidth.ts';

/** `CARD-XREF-RECORD` — copybook CVACT03Y, 50 bytes, key `XREF-CARD-NUM`. */
export interface CardCrossReference {
  readonly cardNumber: string;
  readonly customerId: string;
  readonly accountId: string;
  readonly filler: string;
}

export const CARD_XREF_LENGTH = 50;

export function cardCrossReferenceKey(record: CardCrossReference): string {
  return record.cardNumber;
}

export function parseCardCrossReference(line: string): CardCrossReference {
  const image = padRecord(line, CARD_XREF_LENGTH);
  return {
    cardNumber: readText(image, 1, 16),
    customerId: readDigits(image, 17, 9),
    accountId: readDigits(image, 26, 11),
    filler: readText(image, 37, 14),
  };
}

export function serializeCardCrossReference(record: CardCrossReference): string {
  return assembleRecord(
    [
      writeText(record.cardNumber, 16),
      writeDigits(record.customerId, 9),
      writeDigits(record.accountId, 11),
      writeText(record.filler, 14),
    ],
    CARD_XREF_LENGTH,
  );
}
