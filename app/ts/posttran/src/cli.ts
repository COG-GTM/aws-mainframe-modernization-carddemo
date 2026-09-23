import { pathToFileURL } from 'node:url';

import { closePostingFiles, openPostingFiles } from './io/datasets.ts';
import type { PostingFilePaths } from './io/datasets.ts';
import { FileAbendError } from './io/errors.ts';
import { runPostingJob } from './posting/job.ts';

/**
 * Runnable equivalent of job step `POSTTRAN.STEP15` (EXEC PGM=CBTRN02C).
 * Each DD name of the JCL becomes a `--<ddname>=<path>` option.
 */

const DD_NAMES = ['dalytran', 'xreffile', 'acctfile', 'tcatbalf', 'tranfile', 'dalyrejs'] as const;

type DdName = (typeof DD_NAMES)[number];

const USAGE = `Usage: posttran --dalytran=<file> --xreffile=<file> --acctfile=<file> \\
                --tcatbalf=<file> --tranfile=<file> --dalyrejs=<file>

Every option maps to the DD name of the same name in app/jcl/POSTTRAN.jcl.
ACCTFILE, TCATBALF, TRANFILE and DALYREJS are rewritten in place on success.
Exit code 4 means the run rejected at least one transaction.`;

export function parsePaths(argv: readonly string[]): PostingFilePaths {
  const values = new Map<DdName, string>();
  for (const argument of argv) {
    const match = /^--([a-z]+)=(.+)$/.exec(argument);
    const name = match?.[1];
    const value = match?.[2];
    if (name === undefined || value === undefined) {
      throw new Error(`unrecognised argument: ${argument}\n\n${USAGE}`);
    }
    const ddName = DD_NAMES.find((candidate) => candidate === name);
    if (ddName === undefined) {
      throw new Error(`unknown DD name: --${name}\n\n${USAGE}`);
    }
    values.set(ddName, value);
  }

  const missing = DD_NAMES.filter((ddName) => !values.has(ddName));
  if (missing.length > 0) {
    throw new Error(`missing required DD names: ${missing.join(', ')}\n\n${USAGE}`);
  }

  return {
    dalytran: values.get('dalytran') ?? '',
    xreffile: values.get('xreffile') ?? '',
    acctfile: values.get('acctfile') ?? '',
    tcatbalf: values.get('tcatbalf') ?? '',
    tranfile: values.get('tranfile') ?? '',
    dalyrejs: values.get('dalyrejs') ?? '',
  };
}

export function main(argv: readonly string[], log: (line: string) => void): number {
  const paths = parsePaths(argv);
  const files = openPostingFiles(paths);
  const result = runPostingJob(files, { log });
  closePostingFiles(files, paths);
  return result.returnCode;
}

function runFromCommandLine(): void {
  const log = (line: string): void => {
    process.stdout.write(`${line}\n`);
  };
  try {
    process.exitCode = main(process.argv.slice(2), log);
  } catch (error) {
    // `9999-ABEND-PROGRAM` issues `CALL 'CEE3ABD'`, a hard abend (spec §6.3).
    const prefix = error instanceof FileAbendError ? 'ABEND: ' : '';
    log(`${prefix}${error instanceof Error ? error.message : String(error)}`);
    process.exitCode = 12;
  }
}

const entryPoint = process.argv[1];
if (entryPoint !== undefined && import.meta.url === pathToFileURL(entryPoint).href) {
  runFromCommandLine();
}
