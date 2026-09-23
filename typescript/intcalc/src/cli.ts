#!/usr/bin/env node
import { realpathSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

import { runCbact04c } from './cbact04c.ts';
import { AbendError } from './errors.ts';
import {
  AccountStore,
  DiscgrpStore,
  TransactionWriter,
  XrefStore,
  readTranCatBalFile,
  type LineTerminator,
} from './io.ts';

/** Process exit code used for a CEE3ABD U999 abend (exit codes are byte sized). */
export const ABEND_EXIT_CODE = 99;

interface CliOptions {
  parmDate: string;
  tcatbal: string;
  xref: string;
  discgrp: string;
  account: string;
  accountOut: string;
  transactOut: string;
  lineTerminator: LineTerminator;
  quiet: boolean;
}

const USAGE = `Usage: intcalc --parm <PARM> --tcatbal <file> --xref <file> --discgrp <file> \\
                 --account <file> --transact-out <file> [options]

Equivalent of JCL job INTCALC (//STEP15 EXEC PGM=CBACT04C,PARM='2022071800').

Required:
  --parm <PARM>           Run-date PARM, e.g. 2022071800 (used only as the
                          first 10 bytes of every generated TRAN-ID)
  --tcatbal <file>        TCATBALF  transaction category balance file (LRECL 50)
  --xref <file>           XREFFILE  card cross-reference file (LRECL 50)
  --discgrp <file>        DISCGRP   disclosure group file (LRECL 50)
  --account <file>        ACCTFILE  account master (LRECL 300), read and rewritten
  --transact-out <file>   TRANSACT  generated SYSTRAN transactions (LRECL 350)

Options:
  --account-out <file>    Write updated accounts here instead of in place
  --line-terminator <t>   lf (default), crlf, or none for a raw RECFM=F image
  --quiet                 Suppress the program's DISPLAY output
  --help                  Show this message
`;

function parseLineTerminator(value: string): LineTerminator {
  switch (value) {
    case 'lf':
      return '\n';
    case 'crlf':
      return '\r\n';
    case 'none':
      return '';
    default:
      throw new Error(`invalid --line-terminator '${value}' (expected lf, crlf or none)`);
  }
}

export function parseArgs(argv: readonly string[]): CliOptions {
  const values = new Map<string, string>();
  const flags = new Set<string>();

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === undefined || !arg.startsWith('--')) {
      throw new Error(`unexpected argument '${String(arg)}'`);
    }
    const name = arg.slice(2);
    if (name === 'quiet' || name === 'help') {
      flags.add(name);
      continue;
    }
    const value = argv[index + 1];
    if (value === undefined) {
      throw new Error(`missing value for --${name}`);
    }
    values.set(name, value);
    index += 1;
  }

  if (flags.has('help')) {
    throw new Error(USAGE);
  }

  const required = (name: string): string => {
    const value = values.get(name);
    if (value === undefined) {
      throw new Error(`missing required option --${name}\n\n${USAGE}`);
    }
    return value;
  };

  const account = required('account');
  return {
    parmDate: required('parm'),
    tcatbal: required('tcatbal'),
    xref: required('xref'),
    discgrp: required('discgrp'),
    account,
    accountOut: values.get('account-out') ?? account,
    transactOut: required('transact-out'),
    lineTerminator: parseLineTerminator(values.get('line-terminator') ?? 'lf'),
    quiet: flags.has('quiet'),
  };
}

export function main(argv: readonly string[]): number {
  let options: CliOptions;
  try {
    options = parseArgs(argv);
  } catch (error) {
    process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
    return 2;
  }

  const accounts = AccountStore.fromFile(options.account);
  const transactions = new TransactionWriter();

  try {
    const result = runCbact04c({
      parmDate: options.parmDate,
      tranCatBalRecords: readTranCatBalFile(options.tcatbal),
      accounts,
      xref: XrefStore.fromFile(options.xref),
      discgrp: DiscgrpStore.fromFile(options.discgrp),
      transactions,
      display: options.quiet ? undefined : (line: string): void => void process.stdout.write(`${line}\n`),
    });
    accounts.writeToFile(options.accountOut, options.lineTerminator);
    transactions.writeToFile(options.transactOut, options.lineTerminator);
    process.stdout.write(
      `RECORDS READ: ${String(result.recordCount)} ` +
        `TRANSACTIONS WRITTEN: ${String(result.transactionsWritten)} ` +
        `ACCOUNTS UPDATED: ${String(result.accountsUpdated)}\n`,
    );
    return 0;
  } catch (error) {
    if (error instanceof AbendError) {
      // The COBOL abends with CEE3ABD code 999 and catalogues nothing: the new
      // SYSTRAN generation is deleted (DISP=(NEW,CATLG,DELETE)) while account
      // rewrites already applied stay applied (defect D6).
      process.stdout.write(`${error.message}\nABENDING PROGRAM\n`);
      accounts.writeToFile(options.accountOut, options.lineTerminator);
      return ABEND_EXIT_CODE;
    }
    throw error;
  }
}

const entryPoint = process.argv[1];
if (entryPoint !== undefined && realpathSync(entryPoint) === realpathSync(fileURLToPath(import.meta.url))) {
  process.exitCode = main(process.argv.slice(2));
}
