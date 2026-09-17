/**
 * CBACT02C — read and print the card master file.
 */

import { cardCodec, type CardRecord } from "@carddemo/domain";
import { dataFiles, openCardFile } from "@carddemo/vsam";

import { consoleDisplay, type ProgramOptions, type ProgramResult } from "./program.js";
import { runReaderProgram } from "./reader-program.js";

export function runCbact02c(options: ProgramOptions = {}): ProgramResult {
  const display = options.display ?? consoleDisplay;
  return runReaderProgram<CardRecord>(
    {
      programName: "CBACT02C",
      openErrorMessage: "ERROR OPENING CARDFILE",
      readErrorMessage: "ERROR READING CARDFILE",
      closeErrorMessage: "ERROR CLOSING CARDFILE",
      store: openCardFile(options.path ?? dataFiles.carddata),
      displayInMainLoop: (record, out) => out(cardCodec.encode(record)),
    },
    display,
  );
}
