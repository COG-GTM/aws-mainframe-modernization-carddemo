/**
 * `COCRDLIC` — the credit card list screen: filter by account and/or card
 * number, page forward with PF8 and backward with PF7, and select a line with
 * `S` to view it or `U` to update it.
 */

import type { CardRecord } from "@carddemo/domain";
import type { Ksds } from "@carddemo/vsam";

import {
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
import { editAccountFilter, editCardFilter, type FilterEdit } from "./filters.js";
import { readBackwards, readForward, type BrowseFilters, type CardKey } from "./browse.js";
import { listMessages } from "./messages.js";
import {
  MAX_SCREEN_LINES,
  emptyCardListRow,
  type CardListInput,
  type CardListRow,
  type CardListScreen,
} from "./screens.js";

/** `WS-THIS-PROGCOMMAREA` of `COCRDLIC`. */
export interface CardListState {
  readonly lastKey: CardKey;
  readonly firstKey: CardKey;
  /** `WS-CA-SCREEN-NUM`, the page number shown on the map. */
  readonly screenNum: number;
  /** `CA-LAST-PAGE-SHOWN` when true. */
  readonly lastPageShown: boolean;
  /** `CA-NEXT-PAGE-EXISTS` when true. */
  readonly nextPageExists: boolean;
}

export interface CardListRequest {
  readonly aid: AidKey;
  readonly screen: CardListInput;
  /** Absent on the first entry into the transaction (`EIBCALEN = 0`). */
  readonly commarea?: CardDemoCommarea;
  readonly state?: CardListState;
}

export interface CardListResult extends HandlerOutcome<CardListScreen> {
  readonly state: CardListState;
}

export interface CardListDependencies {
  readonly cardFile: Ksds<CardRecord>;
  readonly now?: Date;
}

export const emptyCardListInput = (): CardListInput => ({
  acctsId: "",
  cardsId: "",
  selects: Array.from({ length: MAX_SCREEN_LINES }, () => ""),
});

export const initialCardListState = (): CardListState => ({
  lastKey: { cardNum: "", acctId: "" },
  firstKey: { cardNum: "", acctId: "" },
  screenNum: 1,
  lastPageShown: false,
  nextPageExists: false,
});

const validAids: readonly AidKey[] = ["ENTER", "PFK03", "PFK07", "PFK08"];

const selectionOf = (raw: string): "blank" | "view" | "update" | "invalid" => {
  const value = raw.trim();
  if (value.length === 0) {
    return "blank";
  }
  if (value === "S" || value === "s") {
    return "view";
  }
  if (value === "U" || value === "u") {
    return "update";
  }
  return "invalid";
};

interface EditedInputs {
  readonly acct: FilterEdit;
  readonly card: FilterEdit;
  readonly inputError: boolean;
  readonly errorMsg: string;
  readonly selectedRow: number;
  readonly selectedAction: "view" | "update" | "none";
}

/** `2200-EDIT-INPUTS`. */
const editInputs = (screen: CardListInput): EditedInputs => {
  const acct = editAccountFilter(screen.acctsId);
  const card = editCardFilter(screen.cardsId);

  let inputError = false;
  let errorMsg = "";
  if (acct.flag === "notOk") {
    inputError = true;
    errorMsg = listMessages.acctFilterNotNumeric;
  }
  if (card.flag === "notOk") {
    inputError = true;
    if (errorMsg.length === 0) {
      errorMsg = listMessages.cardFilterNotNumeric;
    }
  }

  if (inputError) {
    return { acct, card, inputError, errorMsg, selectedRow: 0, selectedAction: "none" };
  }

  const actions = screen.selects.slice(0, MAX_SCREEN_LINES).map((value) => selectionOf(value));
  const chosen = actions.filter((action) => action === "view" || action === "update");
  if (chosen.length > 1) {
    return {
      acct,
      card,
      inputError: true,
      errorMsg: listMessages.moreThanOneAction,
      selectedRow: 0,
      selectedAction: "none",
    };
  }

  let selectedRow = 0;
  let selectedAction: "view" | "update" | "none" = "none";
  actions.forEach((action, index) => {
    if (action === "view" || action === "update") {
      selectedRow = index + 1;
      selectedAction = action;
    } else if (action === "invalid") {
      inputError = true;
      if (errorMsg.length === 0) {
        errorMsg = listMessages.invalidActionCode;
      }
    }
  });

  return { acct, card, inputError, errorMsg, selectedRow, selectedAction };
};

/** `1400-SETUP-MESSAGE`. */
const setupMessages = (
  aid: AidKey,
  edited: EditedInputs,
  state: CardListState,
  errorMsg: string,
  noRecordsFound: boolean,
): { infoMsg: string; errMsg: string; lastPageShown: boolean } => {
  let lastPageShown = state.lastPageShown;
  let infoMsg = "";
  let errMsg = errorMsg;

  if (edited.acct.flag === "notOk" || edited.card.flag === "notOk") {
    // Filter messages already sit in the error line.
  } else if (aid === "PFK07" && state.screenNum === 1) {
    errMsg = listMessages.noPreviousPages;
  } else if (aid === "PFK08" && !state.nextPageExists && state.lastPageShown) {
    errMsg = listMessages.noMorePages;
  } else if (aid === "PFK08" && !state.nextPageExists) {
    infoMsg = listMessages.informRecordActions;
    lastPageShown = true;
  } else if (state.nextPageExists) {
    infoMsg = listMessages.informRecordActions;
  }

  if (noRecordsFound) {
    infoMsg = "";
  }

  return { infoMsg, errMsg, lastPageShown };
};

const renderRows = (rows: readonly CardListRow[], selects: readonly string[]): CardListRow[] =>
  rows.map((row, index) =>
    row.cardNum.length === 0 ? emptyCardListRow() : { ...row, select: selects[index] ?? "" },
  );

/** `COCRDLIC` `0000-MAIN`. */
export const handleCardList = (
  request: CardListRequest,
  dependencies: CardListDependencies,
): CardListResult => {
  const firstEntry = request.commarea === undefined;
  let commarea: CardDemoCommarea = request.commarea ?? {
    ...emptyCommarea(),
    fromTranId: tranIds.cardList,
    fromProgram: programs.cardList,
    lastMap: maps.cardList,
    lastMapSet: mapSets.cardList,
  };
  let state = request.state ?? initialCardListState();

  // "If coming in from menu. Lets forget the past and start afresh."
  const freshStart =
    firstEntry ||
    (commarea.programContext === "enter" && commarea.fromProgram !== programs.cardList);
  if (freshStart) {
    state = initialCardListState();
    commarea = { ...commarea, lastMap: maps.cardList };
  }

  const cameFromSelf = !firstEntry && commarea.fromProgram === programs.cardList;
  const aid: AidKey = validAids.includes(request.aid) ? request.aid : "ENTER";
  const edited = cameFromSelf ? editInputs(request.screen) : editInputs(emptyCardListInput());
  const filters: BrowseFilters = { acct: edited.acct, card: edited.card };

  const header = screenHeader(tranIds.cardList, programs.cardList, dependencies.now ?? new Date());
  const finish = (
    nextState: CardListState,
    screen: CardListScreen | undefined,
    next: { program: string; mapSet: string; map: string },
    transferControl: boolean,
    updatedCommarea: CardDemoCommarea,
  ): CardListResult => ({
    state: nextState,
    commarea: {
      ...updatedCommarea,
      fromTranId: tranIds.cardList,
      fromProgram: programs.cardList,
      lastMapSet: mapSets.cardList,
      lastMap: maps.cardList,
    },
    nextProgram: next.program,
    nextMapSet: next.mapSet,
    nextMap: next.map,
    transferControl,
    ...(screen === undefined ? {} : { screen }),
  });

  // PF3 from this screen returns to the main menu.
  if (aid === "PFK03" && cameFromSelf) {
    return finish(
      state,
      undefined,
      { program: programs.menu, mapSet: mapSets.menu, map: maps.cardList },
      true,
      {
        ...commarea,
        userType: "U",
        programContext: "enter",
        toProgram: programs.menu,
      },
    );
  }

  if (aid !== "PFK08") {
    state = { ...state, lastPageShown: false };
  }

  const page = (
    startKey: CardKey,
    backwards: boolean,
    screenNum: number,
  ): { state: CardListState; rows: CardListRow[]; errorMsg: string; noRecordsFound: boolean } => {
    const result = backwards
      ? readBackwards(dependencies.cardFile, startKey, filters, screenNum)
      : readForward(dependencies.cardFile, startKey.cardNum, filters, screenNum);
    return {
      state: {
        firstKey: result.firstKey,
        lastKey: result.lastKey,
        screenNum: result.screenNum,
        lastPageShown: state.lastPageShown,
        nextPageExists: result.nextPageExists,
      },
      rows: [...result.rows],
      errorMsg: result.errorMsg,
      noRecordsFound: result.noRecordsFound,
    };
  };

  const sendMap = (
    browsed: { state: CardListState; rows: CardListRow[]; errorMsg: string; noRecordsFound: boolean },
    updatedCommarea: CardDemoCommarea,
  ): CardListResult => {
    const messages = setupMessages(
      aid,
      edited,
      browsed.state,
      browsed.errorMsg.length > 0 ? browsed.errorMsg : edited.errorMsg,
      browsed.noRecordsFound,
    );
    const nextState = { ...browsed.state, lastPageShown: messages.lastPageShown };
    const screen: CardListScreen = {
      ...header,
      pageNo: String(nextState.screenNum),
      acctsId: edited.acct.flag === "blank" ? "" : request.screen.acctsId.trim(),
      cardsId: edited.card.flag === "blank" ? "" : request.screen.cardsId.trim(),
      rows: renderRows(browsed.rows, request.screen.selects),
      infoMsg: messages.infoMsg,
      errMsg: messages.errMsg,
    };
    return finish(
      nextState,
      screen,
      { program: programs.cardList, mapSet: mapSets.cardList, map: maps.cardList },
      false,
      updatedCommarea,
    );
  };

  const commareaWithFilters: CardDemoCommarea = {
    ...commarea,
    acctId: edited.acct.flag === "valid" ? edited.acct.value : ZERO_ACCT_ID,
    cardNum: edited.card.flag === "valid" ? edited.card.value : ZERO_CARD_NUM,
  };

  if (edited.inputError) {
    const browsed =
      edited.acct.flag === "notOk" || edited.card.flag === "notOk"
        ? { state, rows: Array.from({ length: MAX_SCREEN_LINES }, emptyCardListRow), errorMsg: "", noRecordsFound: false }
        : page(state.firstKey, false, state.screenNum);
    return sendMap({ ...browsed, errorMsg: edited.errorMsg }, commareaWithFilters);
  }

  if (aid === "PFK07" && state.screenNum === 1) {
    return sendMap(page(state.firstKey, false, state.screenNum), commareaWithFilters);
  }

  if (aid === "PFK03" || (commarea.programContext === "reenter" && !cameFromSelf)) {
    state = initialCardListState();
    return sendMap(page(state.firstKey, false, state.screenNum), {
      ...emptyCommarea(),
      programContext: "enter",
    });
  }

  if (aid === "PFK08" && state.nextPageExists) {
    return sendMap(page(state.lastKey, false, state.screenNum + 1), commareaWithFilters);
  }

  if (aid === "PFK07" && state.screenNum > 1) {
    return sendMap(page(state.firstKey, true, state.screenNum - 1), commareaWithFilters);
  }

  if (aid === "ENTER" && edited.selectedAction !== "none" && cameFromSelf) {
    const row = request.screen.selects[edited.selectedRow - 1];
    const browsed = page(state.firstKey, false, state.screenNum);
    const selected = browsed.rows[edited.selectedRow - 1];
    if (row !== undefined && selected !== undefined && selected.cardNum.length > 0) {
      const detail = edited.selectedAction === "view";
      return finish(
        browsed.state,
        undefined,
        detail
          ? { program: programs.cardDetail, mapSet: mapSets.cardDetail, map: maps.cardDetail }
          : { program: programs.cardUpdate, mapSet: mapSets.cardUpdate, map: maps.cardUpdate },
        true,
        {
          ...commareaWithFilters,
          userType: "U",
          programContext: "enter",
          acctId: selected.acctNo,
          cardNum: selected.cardNum,
        },
      );
    }
  }

  return sendMap(page(state.firstKey, false, state.screenNum), commareaWithFilters);
};
