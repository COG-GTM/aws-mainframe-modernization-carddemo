/** The `CCDA-SCREEN-TITLE` literals from `COTTL01Y`. */
export const CARDDEMO_TITLE_01 = "      AWS Mainframe Modernization       ";
export const CARDDEMO_TITLE_02 = "              CardDemo                  ";
export const CARDDEMO_THANK_YOU = "Thank you for using CCDA application... ";

/** `CURDATE` as the `mm/dd/yy` the maps show. */
export function formatDate(now: Date): string {
  const month = String(now.getMonth() + 1).padStart(2, "0");
  const day = String(now.getDate()).padStart(2, "0");
  const year = String(now.getFullYear() % 100).padStart(2, "0");
  return `${month}/${day}/${year}`;
}

/** `CURTIME` as the `hh:mm:ss` the maps show. */
export function formatTime(now: Date): string {
  const hours = String(now.getHours()).padStart(2, "0");
  const minutes = String(now.getMinutes()).padStart(2, "0");
  const seconds = String(now.getSeconds()).padStart(2, "0");
  return `${hours}:${minutes}:${seconds}`;
}
