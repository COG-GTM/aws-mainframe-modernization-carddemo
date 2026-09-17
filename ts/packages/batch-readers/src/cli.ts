/**
 * Job step entry point: the first argument is the input dataset, replacing the
 * DD statement in the JCL.
 */

import { AbendError, type ProgramOptions, type ProgramResult } from "./program.js";

export function runCli(
  run: (options: ProgramOptions) => ProgramResult,
  argv: readonly string[] = process.argv.slice(2),
): void {
  const [path] = argv;
  try {
    run(path === undefined ? {} : { path });
  } catch (error) {
    if (error instanceof AbendError) {
      process.exitCode = 1;
      return;
    }
    throw error;
  }
}
