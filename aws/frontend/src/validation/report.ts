import type { ReportType } from '../api/types';
import { blank, isDigits, isValidIsoDate } from './common';

export interface ReportForm {
  reportType: ReportType | '';
  sdtMm: string;
  sdtDd: string;
  sdtYyyy: string;
  edtMm: string;
  edtDd: string;
  edtYyyy: string;
  confirm: string;
}

export type ReportField = keyof ReportForm;

export const EMPTY_REPORT_FORM: ReportForm = {
  reportType: '',
  sdtMm: '',
  sdtDd: '',
  sdtYyyy: '',
  edtMm: '',
  edtDd: '',
  edtYyyy: '',
  confirm: '',
};

export const REPORT_NAMES: Record<ReportType, string> = { MONTHLY: 'Monthly', YEARLY: 'Yearly', CUSTOM: 'Custom' };

export const toIso = (y: string, m: string, d: string) => `${y.trim()}-${m.trim().padStart(2, '0')}-${d.trim().padStart(2, '0')}`;

/** CORPT00C PROCESS-ENTER-KEY date edits for the custom range (first error wins). */
export function validateReport(f: ReportForm): { field: ReportField; message: string } | null {
  if (!f.reportType) return { field: 'reportType', message: 'Select a report type to print report...' };
  if (f.reportType !== 'CUSTOM') return null;

  const empties: [ReportField, string][] = [
    ['sdtMm', 'Start Date - Month'],
    ['sdtDd', 'Start Date - Day'],
    ['sdtYyyy', 'Start Date - Year'],
    ['edtMm', 'End Date - Month'],
    ['edtDd', 'End Date - Day'],
    ['edtYyyy', 'End Date - Year'],
  ];
  for (const [field, label] of empties) {
    if (blank(f[field])) return { field, message: `${label} can NOT be empty...` };
  }
  const month = (v: string) => isDigits(v) && Number(v) <= 12;
  const day = (v: string) => isDigits(v) && Number(v) <= 31;
  if (!month(f.sdtMm)) return { field: 'sdtMm', message: 'Start Date - Not a valid Month...' };
  if (!day(f.sdtDd)) return { field: 'sdtDd', message: 'Start Date - Not a valid Day...' };
  if (!isDigits(f.sdtYyyy)) return { field: 'sdtYyyy', message: 'Start Date - Not a valid Year...' };
  if (!month(f.edtMm)) return { field: 'edtMm', message: 'End Date - Not a valid Month...' };
  if (!day(f.edtDd)) return { field: 'edtDd', message: 'End Date - Not a valid Day...' };
  if (!isDigits(f.edtYyyy)) return { field: 'edtYyyy', message: 'End Date - Not a valid Year...' };
  if (!isValidIsoDate(toIso(f.sdtYyyy.padStart(4, '0'), f.sdtMm, f.sdtDd))) {
    return { field: 'sdtMm', message: 'Start Date - Not a valid date...' };
  }
  if (!isValidIsoDate(toIso(f.edtYyyy.padStart(4, '0'), f.edtMm, f.edtDd))) {
    return { field: 'edtMm', message: 'End Date - Not a valid date...' };
  }
  return null;
}
