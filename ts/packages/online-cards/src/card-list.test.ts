import { openCardFile } from "@carddemo/vsam";
import { describe, expect, it } from "vitest";

import {
  emptyCardListInput,
  handleCardList,
  type CardListRequest,
  type CardListResult,
} from "./card-list.js";
import { emptyCommarea, programs } from "./commarea.js";
import { listMessages } from "./messages.js";
import type { CardListRow } from "./screens.js";

const cards = (): ReturnType<typeof openCardFile> => {
  const file = openCardFile();
  file.openFile();
  return file;
};

const allCards = (): { cardNum: string; acctId: string }[] => {
  const file = cards();
  return file.toArray().map((record) => ({
    cardNum: record.cardNum,
    acctId: String(record.cardAcctId).padStart(11, "0"),
  }));
};

const filled = (rows: readonly CardListRow[]): CardListRow[] =>
  rows.filter((row) => row.cardNum.length > 0);

const firstEntry = (): CardListResult =>
  handleCardList({ aid: "ENTER", screen: emptyCardListInput() }, { cardFile: cards() });

const again = (previous: CardListResult, request: Partial<CardListRequest>): CardListResult =>
  handleCardList(
    {
      aid: "ENTER",
      screen: emptyCardListInput(),
      commarea: previous.commarea,
      state: previous.state,
      ...request,
    },
    { cardFile: cards() },
  );

describe("handleCardList", () => {
  it("shows the first seven cards on a fresh entry", () => {
    const result = firstEntry();
    const expected = allCards().slice(0, 7);

    expect(result.screen?.pageNo).toBe("1");
    expect(filled(result.screen?.rows ?? []).map((row) => row.cardNum)).toEqual(
      expected.map((card) => card.cardNum),
    );
    expect(result.screen?.infoMsg).toBe(listMessages.informRecordActions);
    expect(result.screen?.errMsg).toBe("");
  });

  it("pages forwards and backwards over the card file", () => {
    const page1 = firstEntry();
    const page2 = again(page1, { aid: "PFK08" });
    const expected = allCards();

    expect(page2.screen?.pageNo).toBe("2");
    expect(filled(page2.screen?.rows ?? []).map((row) => row.cardNum)).toEqual(
      expected.slice(7, 14).map((card) => card.cardNum),
    );

    const page3 = again(page2, { aid: "PFK08" });
    expect(filled(page3.screen?.rows ?? []).map((row) => row.cardNum)).toEqual(
      expected.slice(14, 21).map((card) => card.cardNum),
    );

    const backTo2 = again(page3, { aid: "PFK07" });
    expect(backTo2.screen?.pageNo).toBe("2");
    expect(filled(backTo2.screen?.rows ?? []).map((row) => row.cardNum)).toEqual(
      expected.slice(7, 14).map((card) => card.cardNum),
    );

    const backTo1 = again(backTo2, { aid: "PFK07" });
    expect(backTo1.screen?.pageNo).toBe("1");
    expect(filled(backTo1.screen?.rows ?? []).map((row) => row.cardNum)).toEqual(
      expected.slice(0, 7).map((card) => card.cardNum),
    );
  });

  it("refuses to page back from the first page", () => {
    const page1 = firstEntry();
    const result = again(page1, { aid: "PFK07" });
    expect(result.screen?.errMsg).toBe(listMessages.noPreviousPages);
  });

  it("stops at the last page", () => {
    let result = firstEntry();
    for (let index = 0; index < 20; index += 1) {
      result = again(result, { aid: "PFK08" });
    }
    expect(result.screen?.errMsg).toBe(listMessages.noMorePages);
  });

  it("filters the list by account number", () => {
    const target = allCards()[3];
    const result = handleCardList(
      {
        aid: "ENTER",
        screen: { ...emptyCardListInput(), acctsId: target?.acctId ?? "" },
        commarea: { ...emptyCommarea(), fromProgram: programs.cardList, programContext: "reenter" },
      },
      { cardFile: cards() },
    );

    const rows = filled(result.screen?.rows ?? []);
    expect(rows).toHaveLength(1);
    expect(rows[0]?.cardNum).toBe(target?.cardNum);
  });

  it("filters the list by card number", () => {
    const target = allCards()[10];
    const result = handleCardList(
      {
        aid: "ENTER",
        screen: { ...emptyCardListInput(), cardsId: target?.cardNum ?? "" },
        commarea: { ...emptyCommarea(), fromProgram: programs.cardList, programContext: "reenter" },
      },
      { cardFile: cards() },
    );

    expect(filled(result.screen?.rows ?? []).map((row) => row.acctNo)).toEqual([target?.acctId]);
  });

  it("reports a filter that matches nothing", () => {
    const result = handleCardList(
      {
        aid: "ENTER",
        screen: { ...emptyCardListInput(), acctsId: "99999999999" },
        commarea: { ...emptyCommarea(), fromProgram: programs.cardList, programContext: "reenter" },
      },
      { cardFile: cards() },
    );
    expect(result.screen?.errMsg).toBe(listMessages.noRecordsFound);
    expect(filled(result.screen?.rows ?? [])).toHaveLength(0);
  });

  it("rejects a filter that is not an eleven digit account number", () => {
    const result = handleCardList(
      {
        aid: "ENTER",
        screen: { ...emptyCardListInput(), acctsId: "123" },
        commarea: { ...emptyCommarea(), fromProgram: programs.cardList, programContext: "reenter" },
      },
      { cardFile: cards() },
    );
    expect(result.screen?.errMsg).toBe(listMessages.acctFilterNotNumeric);
  });

  it("rejects an action code other than S or U", () => {
    const page1 = firstEntry();
    const result = again(page1, {
      screen: { ...emptyCardListInput(), selects: ["X", "", "", "", "", "", ""] },
    });
    expect(result.screen?.errMsg).toBe(listMessages.invalidActionCode);
  });

  it("rejects more than one selected row", () => {
    const page1 = firstEntry();
    const result = again(page1, {
      screen: { ...emptyCardListInput(), selects: ["S", "U", "", "", "", "", ""] },
    });
    expect(result.screen?.errMsg).toBe(listMessages.moreThanOneAction);
  });

  it("transfers to the detail and update programs from a selected row", () => {
    const page1 = firstEntry();
    const target = allCards()[1];

    const view = again(page1, {
      screen: { ...emptyCardListInput(), selects: ["", "S", "", "", "", "", ""] },
    });
    expect(view.transferControl).toBe(true);
    expect(view.nextProgram).toBe(programs.cardDetail);
    expect(view.commarea.cardNum).toBe(target?.cardNum);

    const update = again(page1, {
      screen: { ...emptyCardListInput(), selects: ["", "U", "", "", "", "", ""] },
    });
    expect(update.nextProgram).toBe(programs.cardUpdate);
    expect(update.commarea.acctId).toBe(target?.acctId);
  });

  it("returns to the menu on PF3", () => {
    const page1 = firstEntry();
    const result = again(page1, { aid: "PFK03" });
    expect(result.transferControl).toBe(true);
    expect(result.nextProgram).toBe(programs.menu);
  });
});
