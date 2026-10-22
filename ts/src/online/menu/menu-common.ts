import { cobolString, moveAlnum, type MenuOption, type SymbolicMap } from '../framework/index.js';

/** Option-number parsing shared by COMEN01C / COADM01C PROCESS-ENTER-KEY. */
export function parseOption(optionI: string): { text: string; numeric: boolean; value: number } {
  // PERFORM VARYING WS-IDX FROM LENGTH OF OPTIONI BY -1 UNTIL OPTIONI(WS-IDX:1) NOT = SPACES OR WS-IDX = 1
  let idx = optionI.length;
  while (!(optionI[idx - 1] !== ' ' || idx === 1)) idx--;
  // MOVE OPTIONI(1:WS-IDX) TO WS-OPTION-X (PIC X(02) JUST RIGHT)
  const src = optionI.slice(0, idx);
  let optX = src.length >= 2 ? src.slice(src.length - 2) : src.padStart(2, ' ');
  optX = optX.replace(/ /g, '0');
  // MOVE WS-OPTION-X TO WS-OPTION (PIC 9(02)): the characters are moved unchanged.
  const numeric = /^[0-9]{2}$/.test(optX);
  return { text: optX, numeric, value: numeric ? Number(optX) : Number.NaN };
}

/** BUILD-MENU-OPTIONS: STRING num '. ' name DELIMITED BY SIZE INTO WS-...-OPT-TXT X(40) -> OPTNnnnO. */
export function buildMenuOptions(map: SymbolicMap, options: readonly MenuOption[]): void {
  options.forEach((o, i) => {
    const txt = cobolString([String(o.num).padStart(2, '0'), '. ', moveAlnum(o.name, 35)], moveAlnum('', 40));
    if (i < 12) map.set(`OPTN${String(i + 1).padStart(3, '0')}`, txt);
  });
}
