/**
 * CBCUS01C — read and print the customer master file. The record is displayed
 * twice per read, once by `1000-CUSTFILE-GET-NEXT` and once by the main loop.
 */

import { customerCodec, type CustomerRecord } from "@carddemo/domain";
import { dataFiles, openCustomerFile } from "@carddemo/vsam";

import { consoleDisplay, type ProgramOptions, type ProgramResult } from "./program.js";
import { runReaderProgram } from "./reader-program.js";

export function runCbcus01c(options: ProgramOptions = {}): ProgramResult {
  const display = options.display ?? consoleDisplay;
  return runReaderProgram<CustomerRecord>(
    {
      programName: "CBCUS01C",
      openErrorMessage: "ERROR OPENING CUSTFILE",
      readErrorMessage: "ERROR READING CUSTOMER FILE",
      closeErrorMessage: "ERROR CLOSING CUSTOMER FILE",
      store: openCustomerFile(options.path ?? dataFiles.custdata),
      displayInGetNext: (record, out) => out(customerCodec.encode(record)),
      displayInMainLoop: (record, out) => out(customerCodec.encode(record)),
    },
    display,
  );
}
