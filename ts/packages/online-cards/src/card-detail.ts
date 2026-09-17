/**
 * `COCRDSLC` — the credit card detail screen: read one card by number and
 * display it, either with the keys the card list passed in the commarea or
 * with the keys typed on the map.
 */

import type { CardRecord } from "@carddemo/domain";
import { FileStatus, type Ksds } from "@carddemo/vsam";

import {
  CARD_FILE_NAME,
  ZERO_ACCT_ID,
  ZERO_CARD_NUM,
  emptyCommarea,
  mapSets,
  maps,
  programs,
  screenHeader,
  tranIds,
  type AidKey,
  type CardDemoCommarea,
  type HandlerOutcome,
} from "./commarea.js";
import { editAccountFilter, editCardFilter } from "./filters.js";
import { detailMessages, fileErrorMessage } from "./messages.js";
import type { CardDetailInput, CardDetailScreen } from "./screens.js";

export interface CardDetailRequest {
  readonly aid: AidKey;
  readonly screen: CardDetailInput;
  /** Absent on the first entry into the transaction (`EIBCALEN = 0`). */
  readonly commarea?: CardDemoCommarea;
}

export type CardDetailResult = HandlerOutcome<CardDetailScreen>;

export interface CardDetailDependencies {
  readonly cardFile: Ksds<CardRecord>;
  readonly now?: Date;
}

export const emptyCardDetailInput = (): CardDetailInput => ({ acctsId: "", cardsId: "" });

const expiryParts = (record: CardRecord): { year: string; month: string; day: string } => ({
  year: record.cardExpiraionDate.slice(0, 4),
  month: record.cardExpiraionDate.slice(5, 7),
  day: record.cardExpiraionDate.slice(8, 10),
});

/** `COCRDSLC` `0000-MAIN`. */
export const handleCardDetail = (
  request: CardDetailRequest,
  dependencies: CardDetailDependencies,
): CardDetailResult => {
  const freshStart =
    request.commarea === undefined ||
    (request.commarea.fromProgram === programs.menu &&
      request.commarea.programContext !== "reenter");
  const commarea: CardDemoCommarea = freshStart ? emptyCommarea() : (request.commarea as CardDemoCommarea);
  const aid: AidKey = aidOrEnter(request.aid);

  const header = screenHeader(
    tranIds.cardDetail,
    programs.cardDetail,
    dependencies.now ?? new Date(),
  );

  if (aid === "PFK03") {
    const hasCaller = commarea.fromProgram.trim().length > 0;
    return {
      commarea: {
        ...commarea,
        toTranId: commarea.fromTranId.trim().length > 0 ? commarea.fromTranId : tranIds.menu,
        toProgram: hasCaller ? commarea.fromProgram : programs.menu,
        fromTranId: tranIds.cardDetail,
        fromProgram: programs.cardDetail,
        userType: "U",
        programContext: "enter",
        lastMapSet: mapSets.cardDetail,
        lastMap: maps.cardDetail,
      },
      nextProgram: hasCaller ? commarea.fromProgram : programs.menu,
      nextMapSet: mapSets.cardDetail,
      nextMap: maps.cardDetail,
      transferControl: true,
    };
  }

  const fromList =
    commarea.programContext === "enter" && commarea.fromProgram === programs.cardList;

  const keys = fromList
    ? { acctId: commarea.acctId, cardNum: commarea.cardNum, inputError: false, errorMsg: "" }
    : editKeys(request.screen);

  if (commarea.programContext === "enter" && !fromList) {
    // First display: just prompt for the search keys.
    return sendMap(commarea, header, request.screen, undefined, "", detailMessages.promptForInput);
  }

  if (keys.inputError) {
    return sendMap(commarea, header, request.screen, undefined, keys.errorMsg, "");
  }

  const result = dependencies.cardFile.read(keys.cardNum);
  if (result.status === FileStatus.notFound) {
    return sendMap(
      { ...commarea, acctId: keys.acctId, cardNum: keys.cardNum },
      header,
      request.screen,
      undefined,
      detailMessages.didNotFindAcctCardCombo,
      "",
    );
  }
  if (result.status !== FileStatus.ok || result.record === undefined) {
    return sendMap(
      { ...commarea, acctId: keys.acctId, cardNum: keys.cardNum },
      header,
      request.screen,
      undefined,
      fileErrorMessage("READ", CARD_FILE_NAME, result.status),
      "",
    );
  }

  return sendMap(
    { ...commarea, acctId: keys.acctId, cardNum: keys.cardNum },
    header,
    { acctsId: keys.acctId, cardsId: keys.cardNum },
    result.record,
    "",
    detailMessages.displayingDetails,
  );
};

const aidOrEnter = (aid: AidKey): AidKey => (aid === "ENTER" || aid === "PFK03" ? aid : "ENTER");

/** `2200-EDIT-MAP-INPUTS`. */
const editKeys = (
  screen: CardDetailInput,
): { acctId: string; cardNum: string; inputError: boolean; errorMsg: string } => {
  const acct = editAccountFilter(screen.acctsId);
  const card = editCardFilter(screen.cardsId);

  let errorMsg = "";
  let inputError = false;

  if (acct.flag === "blank") {
    inputError = true;
    errorMsg = detailMessages.promptForAcct;
  } else if (acct.flag === "notOk") {
    inputError = true;
    errorMsg = detailMessages.acctFilterNotNumeric;
  }

  if (card.flag === "blank") {
    inputError = true;
    if (errorMsg.length === 0) {
      errorMsg = detailMessages.promptForCard;
    }
  } else if (card.flag === "notOk") {
    inputError = true;
    if (errorMsg.length === 0) {
      errorMsg = detailMessages.cardFilterNotNumeric;
    }
  }

  if (acct.flag === "blank" && card.flag === "blank") {
    errorMsg = detailMessages.noSearchCriteria;
  }

  return {
    acctId: acct.flag === "valid" ? acct.value : ZERO_ACCT_ID,
    cardNum: card.flag === "valid" ? card.value : ZERO_CARD_NUM,
    inputError,
    errorMsg,
  };
};

/** `1000-SEND-MAP`. */
const sendMap = (
  commarea: CardDemoCommarea,
  header: ReturnType<typeof screenHeader>,
  input: CardDetailInput,
  record: CardRecord | undefined,
  errMsg: string,
  infoMsg: string,
): CardDetailResult => {
  const expiry = record === undefined ? undefined : expiryParts(record);
  const screen: CardDetailScreen = {
    ...header,
    acctsId: input.acctsId.trim(),
    cardsId: input.cardsId.trim(),
    crdName: record?.cardEmbossedName.trimEnd() ?? "",
    crdStcd: record?.cardActiveStatus ?? "",
    expMon: expiry?.month ?? "",
    expYear: expiry?.year ?? "",
    infoMsg,
    errMsg,
  };
  return {
    commarea: {
      ...commarea,
      fromTranId: tranIds.cardDetail,
      fromProgram: programs.cardDetail,
      programContext: "reenter",
      lastMapSet: mapSets.cardDetail,
      lastMap: maps.cardDetail,
    },
    nextProgram: programs.cardDetail,
    nextMapSet: mapSets.cardDetail,
    nextMap: maps.cardDetail,
    transferControl: false,
    screen,
  };
};
