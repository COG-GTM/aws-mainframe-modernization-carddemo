/**
 * CBACT01C — read the account master file and print every record.
 *
 * The COBOL program also builds the OUTFILE, ARRYFILE and VBRCFILE extracts
 * through the `COBDATFT` assembler date routine; only its reader and display
 * behaviour is ported here.
 */

import { accountCodec, type AccountRecord } from "@carddemo/domain";
import { dataFiles, openAccountFile } from "@carddemo/vsam";

import { consoleDisplay, type DisplayLine, type ProgramOptions, type ProgramResult } from "./program.js";
import { recordImage } from "./record-image.js";
import { runReaderProgram } from "./reader-program.js";

function displayAccountFields(record: AccountRecord, display: DisplayLine): void {
  const image = recordImage(accountCodec, record);
  display(`ACCT-ID                 :${image.field("acctId")}`);
  display(`ACCT-ACTIVE-STATUS      :${image.field("acctActiveStatus")}`);
  display(`ACCT-CURR-BAL           :${image.field("acctCurrBal")}`);
  display(`ACCT-CREDIT-LIMIT       :${image.field("acctCreditLimit")}`);
  display(`ACCT-CASH-CREDIT-LIMIT  :${image.field("acctCashCreditLimit")}`);
  display(`ACCT-OPEN-DATE          :${image.field("acctOpenDate")}`);
  display(`ACCT-EXPIRAION-DATE     :${image.field("acctExpiraionDate")}`);
  display(`ACCT-REISSUE-DATE       :${image.field("acctReissueDate")}`);
  display(`ACCT-CURR-CYC-CREDIT    :${image.field("acctCurrCycCredit")}`);
  display(`ACCT-CURR-CYC-DEBIT     :${image.field("acctCurrCycDebit")}`);
  display(`ACCT-GROUP-ID           :${image.field("acctGroupId")}`);
  display("-------------------------------------------------");

  const vbrcRec1 = `${image.field("acctId")}${image.field("acctActiveStatus")}`;
  const vbrcRec2 = [
    image.field("acctId"),
    image.field("acctCurrBal"),
    image.field("acctCreditLimit"),
    image.field("acctReissueDate").slice(0, 4),
  ].join("");
  display(`VBRC-REC1:${vbrcRec1}`);
  display(`VBRC-REC2:${vbrcRec2}`);
}

export function runCbact01c(options: ProgramOptions = {}): ProgramResult {
  const display = options.display ?? consoleDisplay;
  return runReaderProgram<AccountRecord>(
    {
      programName: "CBACT01C",
      openErrorMessage: "ERROR OPENING ACCTFILE",
      readErrorMessage: "ERROR READING ACCOUNT FILE",
      closeErrorMessage: "ERROR CLOSING ACCOUNT FILE",
      store: openAccountFile(options.path ?? dataFiles.acctdata),
      displayInGetNext: displayAccountFields,
      displayInMainLoop: (record, out) => out(accountCodec.encode(record)),
    },
    display,
  );
}
