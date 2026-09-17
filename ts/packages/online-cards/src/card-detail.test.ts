import { openCardFile } from "@carddemo/vsam";
import { describe, expect, it } from "vitest";

import { emptyCardDetailInput, handleCardDetail } from "./card-detail.js";
import { emptyCommarea, programs, tranIds } from "./commarea.js";
import { detailMessages } from "./messages.js";

const cards = (): ReturnType<typeof openCardFile> => {
  const file = openCardFile();
  file.openFile();
  return file;
};

const firstCard = (): { cardNum: string; acctId: string; name: string } => {
  const record = cards().toArray()[0];
  if (record === undefined) {
    throw new Error("no card data");
  }
  return {
    cardNum: record.cardNum,
    acctId: String(record.cardAcctId).padStart(11, "0"),
    name: record.cardEmbossedName.trimEnd(),
  };
};

describe("handleCardDetail", () => {
  it("prompts for the search keys on a fresh entry", () => {
    const result = handleCardDetail(
      { aid: "ENTER", screen: emptyCardDetailInput() },
      { cardFile: cards() },
    );
    expect(result.screen?.infoMsg).toBe(detailMessages.promptForInput);
  });

  it("shows an existing card selected on the list screen", () => {
    const card = firstCard();
    const result = handleCardDetail(
      {
        aid: "ENTER",
        screen: emptyCardDetailInput(),
        commarea: {
          ...emptyCommarea(),
          fromProgram: programs.cardList,
          fromTranId: tranIds.cardList,
          programContext: "enter",
          acctId: card.acctId,
          cardNum: card.cardNum,
        },
      },
      { cardFile: cards() },
    );

    expect(result.screen?.crdName).toBe(card.name);
    expect(result.screen?.cardsId).toBe(card.cardNum);
    expect(result.screen?.errMsg).toBe("");
    expect(result.screen?.infoMsg).toBe(detailMessages.displayingDetails);
  });

  it("shows an existing card entered on the map", () => {
    const card = firstCard();
    const result = handleCardDetail(
      {
        aid: "ENTER",
        screen: { acctsId: card.acctId, cardsId: card.cardNum },
        commarea: {
          ...emptyCommarea(),
          fromProgram: programs.cardDetail,
          programContext: "reenter",
        },
      },
      { cardFile: cards() },
    );
    expect(result.screen?.crdStcd).toBe("Y");
    expect(result.screen?.expYear).toHaveLength(4);
  });

  it("reports a card that is not on file", () => {
    const result = handleCardDetail(
      {
        aid: "ENTER",
        screen: { acctsId: "00000000011", cardsId: "9999999999999999" },
        commarea: {
          ...emptyCommarea(),
          fromProgram: programs.cardDetail,
          programContext: "reenter",
        },
      },
      { cardFile: cards() },
    );
    expect(result.screen?.errMsg).toBe(detailMessages.didNotFindAcctCardCombo);
    expect(result.screen?.crdName).toBe("");
  });

  it("edits the search keys", () => {
    const reenter = { ...emptyCommarea(), fromProgram: programs.cardDetail, programContext: "reenter" } as const;

    expect(
      handleCardDetail(
        { aid: "ENTER", screen: emptyCardDetailInput(), commarea: reenter },
        { cardFile: cards() },
      ).screen?.errMsg,
    ).toBe(detailMessages.noSearchCriteria);

    expect(
      handleCardDetail(
        { aid: "ENTER", screen: { acctsId: "", cardsId: "1234567890123456" }, commarea: reenter },
        { cardFile: cards() },
      ).screen?.errMsg,
    ).toBe(detailMessages.promptForAcct);

    expect(
      handleCardDetail(
        { aid: "ENTER", screen: { acctsId: "00000000011", cardsId: "" }, commarea: reenter },
        { cardFile: cards() },
      ).screen?.errMsg,
    ).toBe(detailMessages.promptForCard);

    expect(
      handleCardDetail(
        { aid: "ENTER", screen: { acctsId: "abc", cardsId: "1234567890123456" }, commarea: reenter },
        { cardFile: cards() },
      ).screen?.errMsg,
    ).toBe(detailMessages.acctFilterNotNumeric);

    expect(
      handleCardDetail(
        { aid: "ENTER", screen: { acctsId: "00000000011", cardsId: "12345" }, commarea: reenter },
        { cardFile: cards() },
      ).screen?.errMsg,
    ).toBe(detailMessages.cardFilterNotNumeric);
  });

  it("returns to the calling program on PF3", () => {
    const result = handleCardDetail(
      {
        aid: "PFK03",
        screen: emptyCardDetailInput(),
        commarea: {
          ...emptyCommarea(),
          fromProgram: programs.cardList,
          fromTranId: tranIds.cardList,
          programContext: "reenter",
        },
      },
      { cardFile: cards() },
    );
    expect(result.transferControl).toBe(true);
    expect(result.nextProgram).toBe(programs.cardList);
  });
});
