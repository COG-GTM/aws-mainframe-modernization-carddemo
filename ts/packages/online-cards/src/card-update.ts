/**
 * `COCRDUPC` — the credit card update screen: fetch a card, edit the embossed
 * name, active status and expiry, confirm the change with PF5 and rewrite the
 * record if nobody else touched it in the meantime.
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
import { fileErrorMessage, updateMessages } from "./messages.js";
import type { CardUpdateInput, CardUpdateScreen } from "./screens.js";

/** `CCUP-CHG-IND`. */
export type CardUpdateStatus =
  | "detailsNotFetched"
  | "showDetails"
  | "changesNotOk"
  | "changesOkNotConfirmed"
  | "changesOkayedAndDone"
  | "changesOkayedLockError"
  | "changesOkayedButFailed";

/** `CCUP-OLD-DETAILS`, the image fetched from the card file. */
export interface CardImage {
  readonly acctId: string;
  readonly cardId: string;
  readonly cvvCd: string;
  readonly crdName: string;
  readonly expYear: string;
  readonly expMon: string;
  readonly expDay: string;
  readonly crdStcd: string;
}

/** `WS-THIS-PROGCOMMAREA` of `COCRDUPC`. */
export interface CardUpdateState {
  readonly status: CardUpdateStatus;
  readonly old: CardImage;
}

export interface CardUpdateRequest {
  readonly aid: AidKey;
  readonly screen: CardUpdateInput;
  /** Absent on the first entry into the transaction (`EIBCALEN = 0`). */
  readonly commarea?: CardDemoCommarea;
  readonly state?: CardUpdateState;
}

export interface CardUpdateResult extends HandlerOutcome<CardUpdateScreen> {
  readonly state: CardUpdateState;
}

export interface CardUpdateDependencies {
  readonly cardFile: Ksds<CardRecord>;
  readonly now?: Date;
}

export const emptyCardUpdateInput = (): CardUpdateInput => ({
  acctsId: "",
  cardsId: "",
  crdName: "",
  crdStcd: "",
  expMon: "",
  expYear: "",
  expDay: "",
});

const emptyImage: CardImage = {
  acctId: "",
  cardId: "",
  cvvCd: "",
  crdName: "",
  expYear: "",
  expMon: "",
  expDay: "",
  crdStcd: "",
};

export const initialCardUpdateState = (): CardUpdateState => ({
  status: "detailsNotFetched",
  old: emptyImage,
});

const changesMade = (status: CardUpdateStatus): boolean =>
  status !== "detailsNotFetched" && status !== "showDetails";

const changesFailed = (status: CardUpdateStatus): boolean =>
  status === "changesOkayedLockError" || status === "changesOkayedButFailed";

const imageOf = (record: CardRecord, acctId: string, cardId: string): CardImage => ({
  acctId,
  cardId,
  cvvCd: String(record.cardCvvCd).padStart(3, "0"),
  crdName: record.cardEmbossedName.trimEnd().toUpperCase(),
  expYear: record.cardExpiraionDate.slice(0, 4),
  expMon: record.cardExpiraionDate.slice(5, 7),
  expDay: record.cardExpiraionDate.slice(8, 10),
  crdStcd: record.cardActiveStatus,
});

const sameCardData = (left: CardImage, right: CardImage): boolean =>
  left.crdName.trimEnd().toUpperCase() === right.crdName.trimEnd().toUpperCase() &&
  left.expYear === right.expYear &&
  left.expMon === right.expMon &&
  left.expDay === right.expDay &&
  left.crdStcd.toUpperCase() === right.crdStcd.toUpperCase();

interface EditedUpdate {
  readonly next: CardImage;
  readonly inputError: boolean;
  readonly errorMsg: string;
  readonly noChanges: boolean;
}

/** `1230-EDIT-NAME` .. `1260-EDIT-EXPIRY-YEAR`. */
const editDetails = (screen: CardUpdateInput, old: CardImage): EditedUpdate => {
  const crdName = screen.crdName === "*" ? "" : screen.crdName;
  const crdStcd = screen.crdStcd === "*" ? "" : screen.crdStcd;
  const expMon = screen.expMon === "*" ? "" : screen.expMon;
  const expYear = screen.expYear === "*" ? "" : screen.expYear;
  const expDay = screen.expDay.trim().length === 0 ? old.expDay : screen.expDay;

  const next: CardImage = {
    acctId: old.acctId,
    cardId: old.cardId,
    cvvCd: old.cvvCd,
    crdName: crdName.trimEnd(),
    expYear: expYear.trim(),
    expMon: expMon.trim(),
    expDay,
    crdStcd: crdStcd.trim(),
  };

  if (sameCardData(next, old)) {
    return { next, inputError: false, errorMsg: updateMessages.noChangesDetected, noChanges: true };
  }

  let errorMsg = "";
  let inputError = false;
  const fail = (message: string): void => {
    inputError = true;
    if (errorMsg.length === 0) {
      errorMsg = message;
    }
  };

  if (next.crdName.trim().length === 0) {
    fail(updateMessages.promptForName);
  } else if (!/^[A-Za-z ]+$/.test(next.crdName)) {
    fail(updateMessages.nameMustBeAlpha);
  }

  if (next.crdStcd.length === 0 || !/^[YN]$/.test(next.crdStcd.toUpperCase())) {
    fail(updateMessages.cardStatusMustBeYesNo);
  }

  const month = Number(next.expMon);
  if (!/^\d+$/.test(next.expMon) || month < 1 || month > 12) {
    fail(updateMessages.cardExpiryMonthNotValid);
  }

  const year = Number(next.expYear);
  if (!/^\d+$/.test(next.expYear) || year < 1950 || year > 2099) {
    fail(updateMessages.cardExpiryYearNotValid);
  }

  return { next, inputError, errorMsg, noChanges: false };
};

/** `9000-READ-DATA` / `9100-GETCARD-BYACCTCARD`. */
const readData = (
  cardFile: Ksds<CardRecord>,
  acctId: string,
  cardId: string,
): { image?: CardImage; errorMsg: string } => {
  const result = cardFile.read(cardId);
  if (result.status === FileStatus.ok && result.record !== undefined) {
    return { image: imageOf(result.record, acctId, cardId), errorMsg: "" };
  }
  if (result.status === FileStatus.notFound) {
    return { errorMsg: updateMessages.didNotFindAcctCardCombo };
  }
  return { errorMsg: fileErrorMessage("READ", CARD_FILE_NAME, result.status) };
};

/** `9200-WRITE-PROCESSING` and `9300-CHECK-CHANGE-IN-REC`. */
const writeProcessing = (
  cardFile: Ksds<CardRecord>,
  old: CardImage,
  next: CardImage,
): { status: CardUpdateStatus; old: CardImage; errorMsg: string } => {
  const current = cardFile.read(next.cardId);
  if (current.status !== FileStatus.ok || current.record === undefined) {
    return {
      status: "changesOkayedLockError",
      old,
      errorMsg: updateMessages.couldNotLockForUpdate,
    };
  }

  const onFile = imageOf(current.record, old.acctId, old.cardId);
  if (!sameCardData(onFile, old) || onFile.cvvCd !== old.cvvCd) {
    return {
      status: "showDetails",
      old: onFile,
      errorMsg: updateMessages.dataWasChangedBeforeUpdate,
    };
  }

  const updated: CardRecord = {
    cardNum: next.cardId,
    cardAcctId: Number(next.acctId),
    cardCvvCd: Number(next.cvvCd),
    cardEmbossedName: next.crdName,
    cardExpiraionDate: `${next.expYear}-${next.expMon}-${next.expDay}`,
    cardActiveStatus: next.crdStcd.toUpperCase(),
  };

  const status = cardFile.rewrite(updated);
  if (status !== FileStatus.ok) {
    return {
      status: "changesOkayedButFailed",
      old,
      errorMsg: updateMessages.lockedButUpdateFailed,
    };
  }

  return { status: "changesOkayedAndDone", old: imageOf(updated, next.acctId, next.cardId), errorMsg: "" };
};

/** `1210-EDIT-ACCOUNT` / `1220-EDIT-CARD`. */
const editSearchKeys = (
  screen: CardUpdateInput,
): { acctId: string; cardId: string; inputError: boolean; errorMsg: string } => {
  const acct = editAccountFilter(screen.acctsId);
  const card = editCardFilter(screen.cardsId);

  let errorMsg = "";
  let inputError = false;

  if (acct.flag === "blank") {
    inputError = true;
    errorMsg = updateMessages.promptForAcct;
  } else if (acct.flag === "notOk") {
    inputError = true;
    errorMsg = updateMessages.acctFilterNotNumeric;
  }

  if (card.flag === "blank") {
    inputError = true;
    if (errorMsg.length === 0) {
      errorMsg = updateMessages.promptForCard;
    }
  } else if (card.flag === "notOk") {
    inputError = true;
    if (errorMsg.length === 0) {
      errorMsg = updateMessages.cardFilterNotNumeric;
    }
  }

  if (acct.flag === "blank" && card.flag === "blank") {
    errorMsg = updateMessages.noSearchCriteria;
  }

  return {
    acctId: acct.flag === "valid" ? acct.value : ZERO_ACCT_ID,
    cardId: card.flag === "valid" ? card.value : ZERO_CARD_NUM,
    inputError,
    errorMsg,
  };
};

/** `3250-SETUP-INFOMSG`. */
const infoMessageFor = (status: CardUpdateStatus, promptForKeys: boolean): string => {
  if (promptForKeys) {
    return updateMessages.promptForSearchKeys;
  }
  switch (status) {
    case "detailsNotFetched":
      return updateMessages.promptForSearchKeys;
    case "showDetails":
      return updateMessages.foundCards;
    case "changesNotOk":
      return updateMessages.promptForChanges;
    case "changesOkNotConfirmed":
      return updateMessages.promptForConfirmation;
    case "changesOkayedAndDone":
      return updateMessages.confirmUpdateSuccess;
    default:
      return updateMessages.informFailure;
  }
};

/** `COCRDUPC` `0000-MAIN`. */
export const handleCardUpdate = (
  request: CardUpdateRequest,
  dependencies: CardUpdateDependencies,
): CardUpdateResult => {
  const firstEntry = request.commarea === undefined;
  const commarea: CardDemoCommarea = request.commarea ?? emptyCommarea();
  let state = request.state ?? initialCardUpdateState();

  const header = screenHeader(
    tranIds.cardUpdate,
    programs.cardUpdate,
    dependencies.now ?? new Date(),
  );

  const aidValid =
    request.aid === "ENTER" ||
    request.aid === "PFK03" ||
    (request.aid === "PFK05" && state.status === "changesOkNotConfirmed") ||
    (request.aid === "PFK12" && state.status !== "detailsNotFetched");
  const aid: AidKey = aidValid ? request.aid : "ENTER";

  const cameFromList = commarea.lastMapSet === mapSets.cardList;

  const sendMap = (
    nextState: CardUpdateState,
    nextCommarea: CardDemoCommarea,
    input: CardUpdateInput,
    errMsg: string,
    promptForKeys: boolean,
  ): CardUpdateResult => {
    const shown =
      nextState.status === "detailsNotFetched"
        ? emptyCardUpdateInput()
        : changesMade(nextState.status)
          ? { ...input, expDay: nextState.old.expDay }
          : {
              ...input,
              crdName: nextState.old.crdName.trimEnd(),
              crdStcd: nextState.old.crdStcd,
              expMon: nextState.old.expMon,
              expYear: nextState.old.expYear,
              expDay: nextState.old.expDay,
            };
    const screen: CardUpdateScreen = {
      ...header,
      acctsId: promptForKeys ? "" : input.acctsId.trim(),
      cardsId: promptForKeys ? "" : input.cardsId.trim(),
      crdName: shown.crdName.trimEnd(),
      crdStcd: shown.crdStcd,
      expMon: shown.expMon,
      expYear: shown.expYear,
      expDay: shown.expDay,
      infoMsg: infoMessageFor(nextState.status, promptForKeys),
      errMsg,
    };
    return {
      state: nextState,
      commarea: {
        ...nextCommarea,
        fromTranId: tranIds.cardUpdate,
        fromProgram: programs.cardUpdate,
        programContext: "reenter",
        lastMapSet: mapSets.cardUpdate,
        lastMap: maps.cardUpdate,
      },
      nextProgram: programs.cardUpdate,
      nextMapSet: mapSets.cardUpdate,
      nextMap: maps.cardUpdate,
      transferControl: false,
      screen,
    };
  };

  if (
    aid === "PFK03" ||
    (state.status === "changesOkayedAndDone" && cameFromList) ||
    (changesFailed(state.status) && cameFromList)
  ) {
    const hasCaller = commarea.fromProgram.trim().length > 0;
    return {
      state,
      commarea: {
        ...commarea,
        toTranId: commarea.fromTranId.trim().length > 0 ? commarea.fromTranId : tranIds.menu,
        toProgram: hasCaller ? commarea.fromProgram : programs.menu,
        fromTranId: tranIds.cardUpdate,
        fromProgram: programs.cardUpdate,
        acctId: cameFromList ? ZERO_ACCT_ID : commarea.acctId,
        cardNum: cameFromList ? ZERO_CARD_NUM : commarea.cardNum,
        userType: "U",
        programContext: "enter",
        lastMapSet: mapSets.cardUpdate,
        lastMap: maps.cardUpdate,
      },
      nextProgram: hasCaller ? commarea.fromProgram : programs.menu,
      nextMapSet: mapSets.cardUpdate,
      nextMap: maps.cardUpdate,
      transferControl: true,
    };
  }

  const fromListEntry =
    commarea.fromProgram === programs.cardList &&
    (commarea.programContext === "enter" || aid === "PFK12");

  if (fromListEntry) {
    const fetched = readData(dependencies.cardFile, commarea.acctId, commarea.cardNum);
    if (fetched.image === undefined) {
      return sendMap(
        { status: "detailsNotFetched", old: emptyImage },
        commarea,
        emptyCardUpdateInput(),
        fetched.errorMsg,
        false,
      );
    }
    return sendMap(
      { status: "showDetails", old: fetched.image },
      commarea,
      { ...emptyCardUpdateInput(), acctsId: commarea.acctId, cardsId: commarea.cardNum },
      "",
      false,
    );
  }

  if (
    firstEntry ||
    (state.status === "detailsNotFetched" && commarea.programContext === "enter") ||
    (commarea.fromProgram === programs.menu && commarea.programContext !== "reenter")
  ) {
    return sendMap(initialCardUpdateState(), commarea, emptyCardUpdateInput(), "", true);
  }

  if (state.status === "changesOkayedAndDone" || changesFailed(state.status)) {
    return sendMap(
      initialCardUpdateState(),
      { ...commarea, acctId: ZERO_ACCT_ID, cardNum: ZERO_CARD_NUM },
      emptyCardUpdateInput(),
      "",
      true,
    );
  }

  // Details are on the screen: edit the inputs and decide what to do next.
  if (state.status === "detailsNotFetched") {
    const keys = editSearchKeys(request.screen);
    if (keys.inputError) {
      return sendMap(state, commarea, request.screen, keys.errorMsg, false);
    }
    const fetched = readData(dependencies.cardFile, keys.acctId, keys.cardId);
    if (fetched.image === undefined) {
      return sendMap(state, commarea, request.screen, fetched.errorMsg, false);
    }
    return sendMap(
      { status: "showDetails", old: fetched.image },
      { ...commarea, acctId: keys.acctId, cardNum: keys.cardId },
      request.screen,
      "",
      false,
    );
  }

  if (aid === "PFK12") {
    const fetched = readData(dependencies.cardFile, state.old.acctId, state.old.cardId);
    const reverted: CardUpdateState =
      fetched.image === undefined ? state : { status: "showDetails", old: fetched.image };
    return sendMap(reverted, commarea, request.screen, fetched.errorMsg, false);
  }

  const edited = editDetails(request.screen, state.old);

  if (state.status === "changesOkNotConfirmed" && aid === "PFK05") {
    const written = writeProcessing(dependencies.cardFile, state.old, edited.next);
    return sendMap(
      { status: written.status, old: written.old },
      commarea,
      request.screen,
      written.errorMsg,
      false,
    );
  }

  if (edited.noChanges) {
    state = { ...state, status: "showDetails" };
    return sendMap(state, commarea, request.screen, edited.errorMsg, false);
  }

  if (edited.inputError) {
    return sendMap(
      { ...state, status: "changesNotOk" },
      commarea,
      request.screen,
      edited.errorMsg,
      false,
    );
  }

  return sendMap(
    { ...state, status: "changesOkNotConfirmed" },
    commarea,
    request.screen,
    "",
    false,
  );
};
