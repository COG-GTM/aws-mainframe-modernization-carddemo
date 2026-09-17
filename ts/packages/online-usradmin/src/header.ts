/**
 * `POPULATE-HEADER-INFO` — the title, transaction, program, date and time
 * fields `COTTL01Y` and `CSDAT01Y` supply to every CardDemo map.
 */

import type { ScreenHeader } from "./screen.js";

/** `CCDA-TITLE01` and `CCDA-TITLE02`. */
export const title01 = "      AWS Mainframe Modernization       ";
export const title02 = "              CardDemo                  ";

const pad2 = (value: number): string => String(value).padStart(2, "0");

/** `WS-CURDATE-MM-DD-YY` and `WS-CURTIME-HH-MM-SS`. */
export function populateHeaderInfo(trnname: string, pgmname: string, now: Date): ScreenHeader {
  return {
    title01,
    title02,
    trnname,
    pgmname,
    curdate: `${pad2(now.getMonth() + 1)}/${pad2(now.getDate())}/${pad2(now.getFullYear() % 100)}`,
    curtime: `${pad2(now.getHours())}:${pad2(now.getMinutes())}:${pad2(now.getSeconds())}`,
  };
}
