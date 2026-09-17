/**
 * CBACT03C — read and print the card cross reference file. The record is
 * displayed twice per read, once by `1000-XREFFILE-GET-NEXT` and once by the
 * main loop.
 */

import { cardXrefCodec, type CardXrefRecord } from "@carddemo/domain";
import { dataFiles, openCardXrefFile } from "@carddemo/vsam";

import { consoleDisplay, type ProgramOptions, type ProgramResult } from "./program.js";
import { runReaderProgram } from "./reader-program.js";

export function runCbact03c(options: ProgramOptions = {}): ProgramResult {
  const display = options.display ?? consoleDisplay;
  return runReaderProgram<CardXrefRecord>(
    {
      programName: "CBACT03C",
      openErrorMessage: "ERROR OPENING XREFFILE",
      readErrorMessage: "ERROR READING XREFFILE",
      closeErrorMessage: "ERROR CLOSING XREFFILE",
      store: openCardXrefFile(options.path ?? dataFiles.cardxref),
      displayInGetNext: (record, out) => out(cardXrefCodec.encode(record)),
      displayInMainLoop: (record, out) => out(cardXrefCodec.encode(record)),
    },
    display,
  );
}
