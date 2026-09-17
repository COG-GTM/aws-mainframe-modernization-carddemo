/**
 * The 3270 attention identifiers the CardDemo programs act on. `EIBAID` is
 * compared against `DFHENTER`, `DFHPF3`, `DFHPF4`, `DFHPF5`, `DFHPF7`,
 * `DFHPF8` and `DFHCLEAR` in the online programs.
 */
export const AID_KEYS = ["ENTER", "PF3", "PF4", "PF5", "PF7", "PF8", "CLEAR"] as const;

export type AidKey = (typeof AID_KEYS)[number];

/** Maps a browser `KeyboardEvent.key` to the AID key it stands in for. */
export function aidKeyForKeyboardEvent(key: string): AidKey | undefined {
  switch (key) {
    case "Enter":
      return "ENTER";
    case "F3":
      return "PF3";
    case "F4":
      return "PF4";
    case "F5":
      return "PF5";
    case "F7":
      return "PF7";
    case "F8":
      return "PF8";
    case "Escape":
      return "CLEAR";
    default:
      return undefined;
  }
}
