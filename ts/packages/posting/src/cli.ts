#!/usr/bin/env node
/**
 * Batch entry point for the posting programs, standing in for the `EXEC PGM=`
 * steps in `POSTTRAN.jcl`.
 *
 *   carddemo-posting CBTRN01C --transaction <path>
 *   carddemo-posting CBTRN02C --transaction <path> --rejects <path>
 */

import { AbendError } from "./abend.js";
import { runDailyTransactionRead } from "./cbtrn01c.js";
import { runDailyTransactionPosting } from "./cbtrn02c.js";

const USAGE =
  "usage: carddemo-posting <CBTRN01C|CBTRN02C> --transaction <path> [--rejects <path>]";

function parseArguments(argv: readonly string[]): Map<string, string> {
  const parsed = new Map<string, string>();
  for (let index = 0; index < argv.length; index += 2) {
    const flag = argv[index];
    const value = argv[index + 1];
    if (flag === undefined || value === undefined || !flag.startsWith("--")) {
      throw new Error(USAGE);
    }
    parsed.set(flag.slice(2), value);
  }
  return parsed;
}

function required(options: Map<string, string>, name: string): string {
  const value = options.get(name);
  if (value === undefined) {
    throw new Error(`missing --${name}\n${USAGE}`);
  }
  return value;
}

export function main(argv: readonly string[]): number {
  const [program, ...rest] = argv;
  const options = parseArguments(rest);
  const log = (line: string): void => {
    process.stdout.write(`${line}\n`);
  };

  switch (program) {
    case "CBTRN01C":
      runDailyTransactionRead({
        transaction: required(options, "transaction"),
        log,
      });
      return 0;
    case "CBTRN02C":
      return runDailyTransactionPosting({
        transaction: required(options, "transaction"),
        rejects: required(options, "rejects"),
        log,
      }).returnCode;
    default:
      throw new Error(USAGE);
  }
}

try {
  process.exitCode = main(process.argv.slice(2));
} catch (error) {
  process.stderr.write(
    `${error instanceof Error ? error.message : String(error)}\n`,
  );
  process.exitCode = error instanceof AbendError ? error.abcode : 12;
}
