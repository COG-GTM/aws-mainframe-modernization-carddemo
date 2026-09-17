/**
 * Resolves the screen a program owns. The online programs are named after
 * their mapset with a `C` suffix — `COSGN00C` sends the single map of the
 * `COSGN00` mapset — which is how CICS `SEND MAP` pairs them up.
 */

import type { ScreenDefinition } from "./bms/types.js";
import { SCREENS_BY_MAP, SCREENS_BY_MAPSET } from "./generated/index.js";

export class UnknownProgramError extends Error {}

/** The mapset a program name refers to, e.g. `COSGN00C` -> `COSGN00`. */
export function mapsetForProgram(program: string): string {
  return program.trim().toUpperCase().slice(0, 7);
}

export function findScreenForProgram(program: string): ScreenDefinition | undefined {
  return SCREENS_BY_MAPSET[mapsetForProgram(program)] ?? SCREENS_BY_MAP[program.trim().toUpperCase()];
}

export function screenForProgram(program: string): ScreenDefinition {
  const screen = findScreenForProgram(program);
  if (screen === undefined) {
    throw new UnknownProgramError(`no BMS map for program ${program}`);
  }
  return screen;
}
