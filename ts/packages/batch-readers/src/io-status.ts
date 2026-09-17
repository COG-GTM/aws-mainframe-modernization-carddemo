/**
 * `9910-DISPLAY-IO-STATUS`: renders a two byte FILE STATUS as the four
 * character `IO-STATUS-04` field the batch programs display.
 */

import type { FileStatusCode } from "@carddemo/vsam";

export function formatIoStatus(status: FileStatusCode | string): string {
  const stat1 = status.charAt(0);
  const stat2 = status.charAt(1);

  if (!/^\d{2}$/.test(status) || stat1 === "9") {
    const binary = String(stat2.charCodeAt(0)).padStart(3, "0");
    return `FILE STATUS IS: NNNN${stat1}${binary}`;
  }

  return `FILE STATUS IS: NNNN00${status}`;
}
