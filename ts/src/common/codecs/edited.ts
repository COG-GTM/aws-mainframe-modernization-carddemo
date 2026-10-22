/**
 * Numeric-edited PICTURE formatting (MOVE numeric -> edited item) and de-editing
 * (MOVE edited -> numeric). Supports 9 Z * . , B 0 / fixed and floating + - $,
 * and trailing CR / DB.
 */
import { Decimal, fromScaledDigits, type NumericInput, toDecimal, toScaledDigits } from './decimal.js';
import { expandPic } from './pic.js';

type Slot =
  | { kind: 'digit'; sym: '9' | 'Z' | '*' | 'float' }
  | { kind: 'point' }
  | { kind: 'insert'; ch: string }
  | { kind: 'sign'; sym: '+' | '-' }
  | { kind: 'currency' }
  | { kind: 'crdb'; text: 'CR' | 'DB' }
  | { kind: 'floatLead' };

interface EditPlan {
  slots: Slot[];
  floatSym: '+' | '-' | '$' | null;
  floatStart: number;
  floatEnd: number;
  intDigits: number;
  fracDigits: number;
  length: number;
}

const planCache = new Map<string, EditPlan>();

function plan(pic: string): EditPlan {
  const cached = planCache.get(pic);
  if (cached) return cached;
  const body = expandPic(pic).replace(/V/g, '');
  const tokens: string[] = [];
  for (let i = 0; i < body.length; i++) {
    const two = body.slice(i, i + 2);
    if (two === 'CR' || two === 'DB') {
      tokens.push(two);
      i++;
    } else tokens.push(body[i]!);
  }
  let floatSym: EditPlan['floatSym'] = null;
  for (const s of ['+', '-', '$'] as const) {
    if (tokens.filter((t) => t === s).length >= 2) floatSym = s;
  }
  const floatStart = floatSym ? tokens.indexOf(floatSym) : -1;
  const floatEnd = floatSym ? tokens.lastIndexOf(floatSym) : -1;
  const slots: Slot[] = [];
  let seenPoint = false;
  let intDigits = 0;
  let fracDigits = 0;
  tokens.forEach((t, i) => {
    if (floatSym && t === floatSym && i >= floatStart && i <= floatEnd) {
      if (i === floatStart) slots.push({ kind: 'floatLead' });
      else {
        slots.push({ kind: 'digit', sym: 'float' });
        if (seenPoint) fracDigits++;
        else intDigits++;
      }
      return;
    }
    switch (t) {
      case '9':
      case 'Z':
      case '*':
        slots.push({ kind: 'digit', sym: t });
        if (seenPoint) fracDigits++;
        else intDigits++;
        break;
      case '.':
        seenPoint = true;
        slots.push({ kind: 'point' });
        break;
      case ',':
        slots.push({ kind: 'insert', ch: ',' });
        break;
      case 'B':
        slots.push({ kind: 'insert', ch: ' ' });
        break;
      case '0':
        slots.push({ kind: 'insert', ch: '0' });
        break;
      case '/':
        slots.push({ kind: 'insert', ch: '/' });
        break;
      case '+':
      case '-':
        slots.push({ kind: 'sign', sym: t });
        break;
      case '$':
        slots.push({ kind: 'currency' });
        break;
      case 'CR':
      case 'DB':
        slots.push({ kind: 'crdb', text: t });
        break;
      default:
        throw new Error(`Unsupported edit symbol '${t}' in PIC ${pic}`);
    }
  });
  const length = slots.reduce((n, s) => n + (s.kind === 'crdb' ? 2 : 1), 0);
  const result: EditPlan = { slots, floatSym, floatStart, floatEnd, intDigits, fracDigits, length };
  planCache.set(pic, result);
  return result;
}

/** Display length of a numeric-edited picture. */
export function editedLength(pic: string): number {
  return plan(pic).length;
}

/** Formats a value into a numeric-edited picture (COBOL MOVE semantics: truncation, no rounding). */
export function formatEdited(pic: string, value: NumericInput): string {
  const p = plan(pic);
  const totalDigits = p.intDigits + p.fracDigits;
  const signedPic = p.slots.some((s) => s.kind === 'sign' || s.kind === 'crdb') || p.floatSym === '+' || p.floatSym === '-';
  const { digitString, negative } = toScaledDigits(value, totalDigits, p.fracDigits, signedPic);
  const isZero = /^0*$/.test(digitString);
  const allSuppressed = p.slots.every((s) => s.kind !== 'digit' || s.sym !== '9');
  const fill = p.slots.some((s) => s.kind === 'digit' && s.sym === '*') ? '*' : ' ';

  if (isZero && allSuppressed) {
    // Whole item is blanked (or asterisk-filled, keeping the decimal point).
    if (fill === '*') return p.slots.map((s) => (s.kind === 'point' ? '.' : s.kind === 'crdb' ? '**' : '*')).join('');
    return ' '.repeat(p.length);
  }

  const signChar = (sym: '+' | '-'): string => (negative ? '-' : sym === '+' ? '+' : ' ');
  const out: string[] = [];
  let digitIdx = 0;
  let started = false;
  let firstSignificantOut = -1;
  for (const slot of p.slots) {
    switch (slot.kind) {
      case 'digit': {
        const d = digitString[digitIdx++]!;
        if (slot.sym === '9' || started || d !== '0') {
          if (!started) firstSignificantOut = out.length;
          started = true;
          out.push(d);
        } else out.push(slot.sym === '*' ? '*' : ' ');
        break;
      }
      case 'point':
        if (!started) firstSignificantOut = out.length;
        started = true;
        out.push('.');
        break;
      case 'insert':
        out.push(started ? slot.ch : fill);
        break;
      case 'sign':
        out.push(signChar(slot.sym));
        break;
      case 'currency':
        out.push('$');
        break;
      case 'crdb':
        out.push(negative ? slot.text : '  ');
        break;
      case 'floatLead':
        out.push(' ');
        break;
    }
  }
  if (p.floatSym) {
    // Output index of the slot just after the floating string.
    const afterFloat = outIndexOfSlot(p, p.floatEnd + 1);
    const firstSig = firstSignificantOut < 0 ? afterFloat : Math.min(firstSignificantOut, afterFloat);
    const symPos = Math.max(firstSig - 1, outIndexOfSlot(p, p.floatStart));
    out[symPos] = p.floatSym === '$' ? '$' : signChar(p.floatSym);
  }
  return out.join('');
}

function outIndexOfSlot(p: EditPlan, slotIndex: number): number {
  let n = 0;
  for (let i = 0; i < slotIndex && i < p.slots.length; i++) n += p.slots[i]!.kind === 'crdb' ? 2 : 1;
  return n;
}

/**
 * De-edits a numeric-edited value back to a number (MOVE edited -> numeric):
 * digits are taken in order, the scale comes from the picture, and the value is
 * negative when a '-' sign, 'CR' or 'DB' is present.
 */
export function deEdit(pic: string, text: string): Decimal {
  const p = plan(pic);
  const digits = text.replace(/[^0-9]/g, '');
  const negative = /-|CR|DB/.test(text);
  if (digits === '') return new Decimal(0);
  const fraction = text.includes('.') ? (text.split('.')[1] ?? '').replace(/[^0-9]/g, '').length : 0;
  const scale = text.includes('.') ? fraction : p.fracDigits;
  return toDecimal(fromScaledDigits(digits, scale, negative));
}
