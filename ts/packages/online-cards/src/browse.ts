/**
 * `9000-READ-FORWARD`, `9100-READ-BACKWARDS` and `9500-FILTER-RECORDS` of
 * `COCRDLIC`, expressed over the `Ksds` browse verbs.
 */

import type { CardRecord } from "@carddemo/domain";
import { FileStatus, type IoResult, type Ksds } from "@carddemo/vsam";

import { ACCT_ID_LENGTH, CARD_FILE_NAME } from "./commarea.js";
import type { FilterEdit } from "./filters.js";
import { fileErrorMessage, listMessages } from "./messages.js";
import { MAX_SCREEN_LINES, emptyCardListRow, type CardListRow } from "./screens.js";

export interface CardKey {
  readonly cardNum: string;
  readonly acctId: string;
}

export interface BrowsePage {
  /** The seven screen lines; unfilled lines stay empty. */
  readonly rows: readonly CardListRow[];
  readonly firstKey: CardKey;
  readonly lastKey: CardKey;
  readonly nextPageExists: boolean;
  readonly errorMsg: string;
  readonly noRecordsFound: boolean;
  /** `9000-READ-FORWARD` bumps a zero page number to one on the first row. */
  readonly screenNum: number;
}

export interface BrowseFilters {
  readonly acct: FilterEdit;
  readonly card: FilterEdit;
}

const acctIdOf = (record: CardRecord): string =>
  String(record.cardAcctId).padStart(ACCT_ID_LENGTH, "0");

const keyOf = (record: CardRecord): CardKey => ({
  cardNum: record.cardNum,
  acctId: acctIdOf(record),
});

const emptyKey: CardKey = { cardNum: "", acctId: "" };

const rowOf = (record: CardRecord): CardListRow => ({
  select: "",
  acctNo: acctIdOf(record),
  cardNum: record.cardNum,
  cardStatus: record.cardActiveStatus,
});

/** `9500-FILTER-RECORDS`. */
const included = (record: CardRecord, filters: BrowseFilters): boolean => {
  if (filters.acct.flag === "valid" && acctIdOf(record) !== filters.acct.value) {
    return false;
  }
  return !(filters.card.flag === "valid" && record.cardNum !== filters.card.value);
};

const readError = (result: IoResult<CardRecord>): string =>
  fileErrorMessage("READ", CARD_FILE_NAME, result.status);

/** `9000-READ-FORWARD`: fill the page from `startKey` onwards. */
export const readForward = (
  cardFile: Ksds<CardRecord>,
  startKey: string,
  filters: BrowseFilters,
  screenNum: number,
): BrowsePage => {
  const rows: CardListRow[] = Array.from({ length: MAX_SCREEN_LINES }, emptyCardListRow);
  cardFile.startBrowse(startKey.trim().length === 0 ? undefined : startKey);

  let page = screenNum;
  let counter = 0;
  let firstKey = emptyKey;
  let lastKey = emptyKey;
  let lastRecord: CardRecord | undefined;
  let nextPageExists = true;
  let errorMsg = "";
  let noRecordsFound = false;

  for (;;) {
    const result = cardFile.readNext();
    if (result.status === FileStatus.ok && result.record !== undefined) {
      const record = result.record;
      lastRecord = record;
      if (included(record, filters)) {
        rows[counter] = rowOf(record);
        counter += 1;
        if (counter === 1) {
          firstKey = keyOf(record);
          if (page === 0) {
            page = 1;
          }
        }
      }
      if (counter === MAX_SCREEN_LINES) {
        lastKey = keyOf(record);
        const lookahead = cardFile.readNext();
        if (lookahead.status === FileStatus.ok && lookahead.record !== undefined) {
          nextPageExists = true;
          lastKey = keyOf(lookahead.record);
        } else if (lookahead.status === FileStatus.endOfFile) {
          nextPageExists = false;
          errorMsg = listMessages.noMoreRecords;
        } else {
          errorMsg = readError(lookahead);
        }
        break;
      }
      continue;
    }

    if (result.status === FileStatus.endOfFile) {
      nextPageExists = false;
      lastKey = lastRecord === undefined ? emptyKey : keyOf(lastRecord);
      errorMsg = listMessages.noMoreRecords;
      if (page === 1 && counter === 0) {
        errorMsg = listMessages.noRecordsFound;
        noRecordsFound = true;
      }
      break;
    }

    errorMsg = readError(result);
    break;
  }

  return { rows, firstKey, lastKey, nextPageExists, errorMsg, noRecordsFound, screenNum: page };
};

/**
 * `9100-READ-BACKWARDS`: fill the page ending just before `startKey`, which is
 * the first card of the page currently displayed. The record the browse is
 * positioned on is read and discarded, exactly as the COBOL does, so the page
 * holds the seven cards preceding it.
 */
export const readBackwards = (
  cardFile: Ksds<CardRecord>,
  startKey: CardKey,
  filters: BrowseFilters,
  screenNum: number,
): BrowsePage => {
  const rows: CardListRow[] = Array.from({ length: MAX_SCREEN_LINES }, emptyCardListRow);
  cardFile.startBrowse(startKey.cardNum.trim().length === 0 ? undefined : startKey.cardNum);

  let firstKey = emptyKey;
  const lastKey: CardKey = startKey;

  const positioned = cardFile.readPrev();
  if (positioned.status !== FileStatus.ok) {
    return {
      rows,
      firstKey,
      lastKey,
      nextPageExists: true,
      errorMsg: readError(positioned),
      noRecordsFound: false,
      screenNum,
    };
  }

  let counter = MAX_SCREEN_LINES;
  let errorMsg = "";

  while (counter > 0) {
    const result = cardFile.readPrev();
    if (result.status !== FileStatus.ok || result.record === undefined) {
      errorMsg = readError(result);
      break;
    }
    const record = result.record;
    if (!included(record, filters)) {
      continue;
    }
    counter -= 1;
    rows[counter] = rowOf(record);
    if (counter === 0) {
      firstKey = keyOf(record);
    }
  }

  return {
    rows,
    firstKey,
    lastKey,
    nextPageExists: true,
    errorMsg,
    noRecordsFound: false,
    screenNum,
  };
};
