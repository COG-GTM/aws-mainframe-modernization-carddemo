import { copyFileSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { dataFiles, openCardFile } from "@carddemo/vsam";
import { describe, expect, it } from "vitest";

import {
  emptyCardUpdateInput,
  handleCardUpdate,
  type CardUpdateResult,
  type CardUpdateState,
} from "./card-update.js";
import { emptyCommarea, programs, tranIds } from "./commarea.js";
import { updateMessages } from "./messages.js";
import type { CardUpdateInput } from "./screens.js";

const cards = (path?: string): ReturnType<typeof openCardFile> => {
  const file = path === undefined ? openCardFile() : openCardFile(path);
  file.openFile();
  return file;
};

const scratchFile = (): ReturnType<typeof openCardFile> => {
  const scratch = join(mkdtempSync(join(tmpdir(), "carddemo-")), "carddata.txt");
  copyFileSync(dataFiles.carddata, scratch);
  return cards(scratch);
};

const firstCard = (): { cardNum: string; acctId: string } => {
  const record = cards().toArray()[0];
  if (record === undefined) {
    throw new Error("no card data");
  }
  return { cardNum: record.cardNum, acctId: String(record.cardAcctId).padStart(11, "0") };
};

/** Enter from the card list, which leaves the details on the screen. */
const shownDetails = (cardFile: ReturnType<typeof openCardFile>): CardUpdateResult => {
  const card = firstCard();
  return handleCardUpdate(
    {
      aid: "ENTER",
      screen: emptyCardUpdateInput(),
      commarea: {
        ...emptyCommarea(),
        fromProgram: programs.cardList,
        fromTranId: tranIds.cardList,
        programContext: "enter",
        acctId: card.acctId,
        cardNum: card.cardNum,
      },
    },
    { cardFile },
  );
};

const submit = (
  previous: CardUpdateResult,
  changes: Partial<CardUpdateInput>,
  cardFile: ReturnType<typeof openCardFile>,
  aid: "ENTER" | "PFK05" = "ENTER",
): CardUpdateResult => {
  const screen = previous.screen;
  if (screen === undefined) {
    throw new Error("expected a screen");
  }
  return handleCardUpdate(
    {
      aid,
      screen: {
        acctsId: screen.acctsId,
        cardsId: screen.cardsId,
        crdName: screen.crdName,
        crdStcd: screen.crdStcd,
        expMon: screen.expMon,
        expYear: screen.expYear,
        expDay: screen.expDay,
        ...changes,
      },
      commarea: previous.commarea,
      state: previous.state,
    },
    { cardFile },
  );
};

const stateOf = (result: CardUpdateResult): CardUpdateState => result.state;

describe("handleCardUpdate", () => {
  it("prompts for the search keys on a fresh entry", () => {
    const result = handleCardUpdate(
      { aid: "ENTER", screen: emptyCardUpdateInput() },
      { cardFile: cards() },
    );
    expect(result.screen?.infoMsg).toBe(updateMessages.promptForSearchKeys);
    expect(stateOf(result).status).toBe("detailsNotFetched");
  });

  it("shows the card selected on the list screen", () => {
    const result = shownDetails(cards());
    expect(stateOf(result).status).toBe("showDetails");
    expect(result.screen?.infoMsg).toBe(updateMessages.foundCards);
    expect(result.screen?.crdStcd).toBe("Y");
  });

  it("reports a card that is not on file", () => {
    const result = handleCardUpdate(
      {
        aid: "ENTER",
        screen: { ...emptyCardUpdateInput(), acctsId: "00000000011", cardsId: "9999999999999999" },
        commarea: { ...emptyCommarea(), fromProgram: programs.cardUpdate, programContext: "reenter" },
        state: { status: "detailsNotFetched", old: stateOf(shownDetails(cards())).old },
      },
      { cardFile: cards() },
    );
    expect(result.screen?.errMsg).toBe(updateMessages.didNotFindAcctCardCombo);
  });

  it("reports when nothing was changed", () => {
    const cardFile = cards();
    const shown = shownDetails(cardFile);
    const result = submit(shown, {}, cardFile);
    expect(result.screen?.errMsg).toBe(updateMessages.noChangesDetected);
    expect(stateOf(result).status).toBe("showDetails");
  });

  it("edits every changed field", () => {
    const cardFile = cards();
    const shown = shownDetails(cardFile);

    expect(submit(shown, { crdName: "" }, cardFile).screen?.errMsg).toBe(
      updateMessages.promptForName,
    );
    expect(submit(shown, { crdName: "John 3rd" }, cardFile).screen?.errMsg).toBe(
      updateMessages.nameMustBeAlpha,
    );
    expect(submit(shown, { crdStcd: "" }, cardFile).screen?.errMsg).toBe(
      updateMessages.cardStatusMustBeYesNo,
    );
    expect(submit(shown, { crdStcd: "X" }, cardFile).screen?.errMsg).toBe(
      updateMessages.cardStatusMustBeYesNo,
    );
    expect(submit(shown, { expMon: "" }, cardFile).screen?.errMsg).toBe(
      updateMessages.cardExpiryMonthNotValid,
    );
    expect(submit(shown, { expMon: "13" }, cardFile).screen?.errMsg).toBe(
      updateMessages.cardExpiryMonthNotValid,
    );
    expect(submit(shown, { expYear: "" }, cardFile).screen?.errMsg).toBe(
      updateMessages.cardExpiryYearNotValid,
    );
    expect(submit(shown, { expYear: "1776" }, cardFile).screen?.errMsg).toBe(
      updateMessages.cardExpiryYearNotValid,
    );

    expect(stateOf(submit(shown, { crdStcd: "X" }, cardFile)).status).toBe("changesNotOk");
  });

  it("asks for confirmation and then rewrites the card", () => {
    const cardFile = scratchFile();
    const shown = shownDetails(cardFile);

    const validated = submit(shown, { crdName: "JANE DOE", crdStcd: "N" }, cardFile);
    expect(validated.screen?.infoMsg).toBe(updateMessages.promptForConfirmation);
    expect(stateOf(validated).status).toBe("changesOkNotConfirmed");

    const saved = submit(validated, {}, cardFile, "PFK05");
    expect(saved.screen?.infoMsg).toBe(updateMessages.confirmUpdateSuccess);
    expect(stateOf(saved).status).toBe("changesOkayedAndDone");

    const written = cardFile.read(firstCard().cardNum).record;
    expect(written?.cardEmbossedName.trimEnd()).toBe("JANE DOE");
    expect(written?.cardActiveStatus).toBe("N");
  });

  it("refuses to save when the record changed underneath", () => {
    const cardFile = scratchFile();
    const shown = shownDetails(cardFile);
    const validated = submit(shown, { crdName: "JANE DOE" }, cardFile);

    const onFile = cardFile.read(firstCard().cardNum).record;
    if (onFile === undefined) {
      throw new Error("expected the card on file");
    }
    cardFile.rewrite({ ...onFile, cardActiveStatus: "N" });

    const saved = submit(validated, {}, cardFile, "PFK05");
    expect(saved.screen?.errMsg).toBe(updateMessages.dataWasChangedBeforeUpdate);
    expect(stateOf(saved).status).toBe("showDetails");
  });

  it("returns to the calling program on PF3", () => {
    const result = handleCardUpdate(
      {
        aid: "PFK03",
        screen: emptyCardUpdateInput(),
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
