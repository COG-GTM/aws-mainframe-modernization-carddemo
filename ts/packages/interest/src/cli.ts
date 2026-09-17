#!/usr/bin/env node
/**
 * Batch entry point for `CBACT04C`, equivalent to the `INTCALC` job step:
 * the `PARM` date, the `ACCTFILE` posted back to and the `TRANSACT` file the
 * interest transactions are written to.
 */

import { argv, exit, stderr } from "node:process";

import { AbendError, runInterestCalculation, type InterestOptions } from "./program.js";

const usage = [
  "usage: carddemo-interest --date <yyyymmddhh> --accounts <path> --transactions <path>",
  "                        [--tcatbal <path>] [--xref <path>] [--discgrp <path>]",
].join("\n");

interface ParsedArguments {
  readonly date?: string;
  readonly accounts?: string;
  readonly transactions?: string;
  readonly tcatbal?: string;
  readonly xref?: string;
  readonly discgrp?: string;
}

const flags = new Set(["date", "accounts", "transactions", "tcatbal", "xref", "discgrp"]);

export function parseArguments(args: readonly string[]): ParsedArguments {
  const parsed: Record<string, string> = {};
  for (let index = 0; index < args.length; index += 2) {
    const flag = (args[index] ?? "").replace(/^--/, "");
    const value = args[index + 1];
    if (!flags.has(flag) || value === undefined) {
      throw new Error(`unrecognised argument: ${args[index] ?? ""}`);
    }
    parsed[flag] = value;
  }
  return parsed;
}

export function main(args: readonly string[]): number {
  let parsed: ParsedArguments;
  try {
    parsed = parseArguments(args);
  } catch (error) {
    stderr.write(`${(error as Error).message}\n${usage}\n`);
    return 12;
  }

  const { date, accounts, transactions } = parsed;
  if (date === undefined || accounts === undefined || transactions === undefined) {
    stderr.write(`${usage}\n`);
    return 12;
  }

  const options: InterestOptions = {
    parmDate: date,
    accountPath: accounts,
    transactionPath: transactions,
    ...(parsed.tcatbal === undefined ? {} : { tcatbalPath: parsed.tcatbal }),
    ...(parsed.xref === undefined ? {} : { xrefPath: parsed.xref }),
    ...(parsed.discgrp === undefined ? {} : { discgrpPath: parsed.discgrp }),
  };

  try {
    runInterestCalculation(options);
  } catch (error) {
    if (error instanceof AbendError) {
      stderr.write(`${error.message}\n`);
      return error.abendCode;
    }
    throw error;
  }
  return 0;
}

exit(main(argv.slice(2)));
