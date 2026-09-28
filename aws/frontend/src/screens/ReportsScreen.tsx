import { useEffect, useRef, useState } from 'react';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { ReportRequest, ReportType } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { blank } from '../validation/common';
import { EMPTY_REPORT_FORM, REPORT_NAMES, toIso, validateReport, type ReportField, type ReportForm } from '../validation/report';

const TYPES: [ReportType, string][] = [
  ['MONTHLY', 'Monthly (Current Month)'],
  ['YEARLY', 'Yearly (Current Year)'],
  ['CUSTOM', 'Custom (Date Range)'],
];

export function ReportsScreen() {
  const back = useBack();
  const focus = useFieldFocus();
  const [form, setForm] = useState<ReportForm>(EMPTY_REPORT_FORM);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [info, setInfo] = useState<string | null>(null);
  const [invalid, setInvalid] = useState<ReportField | null>(null);
  const [busy, setBusy] = useState(false);
  const pollRef = useRef<number | null>(null);

  useEffect(() => () => {
    if (pollRef.current !== null) window.clearTimeout(pollRef.current);
  }, []);

  const error = (field: ReportField, text: string, kind: ScreenMessage['kind'] = 'error') => {
    setInvalid(kind === 'error' ? field : null);
    setMessage({ text, kind });
    focus(field === 'reportType' ? 'rp-MONTHLY' : `rp-${field}`);
  };

  const poll = (requestId: string, attempt = 0) => {
    pollRef.current = window.setTimeout(async () => {
      try {
        const status = await api.reportStatus(requestId);
        setInfo(`Report ${requestId.slice(0, 8)}: ${status.status}${status.reportS3Key ? ` (${status.reportS3Key})` : ''}`);
        if ((status.status === 'SUBMITTED' || status.status === 'RUNNING') && attempt < 20) poll(requestId, attempt + 1);
      } catch {
        setInfo(null);
      }
    }, 1500);
  };

  /** CORPT00C PROCESS-ENTER-KEY + SUBMIT-JOB-TO-INTRDR. */
  const enter = async () => {
    const problem = validateReport(form);
    if (problem) return error(problem.field, problem.message);
    const type = form.reportType as ReportType;
    const name = REPORT_NAMES[type];
    const c = form.confirm.trim();
    if (blank(c)) return error('confirm', `Please confirm to print the ${name} report...`, 'info');
    if (c.toUpperCase() === 'N') {
      setForm(EMPTY_REPORT_FORM);
      setMessage(null);
      setInvalid(null);
      return;
    }
    if (c.toUpperCase() !== 'Y') return error('confirm', `"${c}" is not a valid value to confirm...`);
    const body: ReportRequest =
      type === 'CUSTOM'
        ? {
            reportType: type,
            startDate: toIso(form.sdtYyyy, form.sdtMm, form.sdtDd),
            endDate: toIso(form.edtYyyy, form.edtMm, form.edtDd),
          }
        : { reportType: type };
    setBusy(true);
    try {
      const submitted = await api.submitReport(body);
      setForm(EMPTY_REPORT_FORM);
      setInvalid(null);
      setMessage({ text: submitted.message, kind: 'success' });
      setInfo(`Report ${submitted.requestId.slice(0, 8)}: SUBMITTED (${submitted.startDate} to ${submitted.endDate})`);
      poll(submitted.requestId);
    } catch (err) {
      setMessage({ text: errorMessage(err) || 'Unable to Write TDQ (JOBS)...', kind: 'error' });
    } finally {
      setBusy(false);
    }
  };

  const set = (field: ReportField) => (value: string) => setForm((f) => ({ ...f, [field]: value }));
  const dateInput = (field: ReportField, label: string, len: number) => (
    <input
      id={`rp-${field}`}
      aria-label={label}
      className={`w${len}`}
      value={form[field]}
      onChange={(e) => set(field)(e.target.value.replace(/\D/g, ''))}
      maxLength={len}
      inputMode="numeric"
      aria-invalid={invalid === field || undefined}
    />
  );

  return (
    <Screen
      tranId="CR00"
      program="CORPT00C"
      title="Transaction Reports"
      message={message}
      info={info}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: enter },
        { key: 'F3', label: 'Back', action: back },
      ]}
    >
      <fieldset className="report-types" aria-invalid={invalid === 'reportType' || undefined}>
        <legend>Select report type</legend>
        {TYPES.map(([type, label]) => (
          <label key={type} className="radio">
            <input
              id={`rp-${type}`}
              type="radio"
              name="reportType"
              checked={form.reportType === type}
              onChange={() => setForm((f) => ({ ...f, reportType: type }))}
            />
            {label}
          </label>
        ))}
      </fieldset>
      <div className={`custom-range ${form.reportType === 'CUSTOM' ? '' : 'muted'}`}>
        <fieldset className="field date-group">
          <legend>Start Date :</legend>
          {dateInput('sdtMm', 'Start month', 2)} / {dateInput('sdtDd', 'Start day', 2)} / {dateInput('sdtYyyy', 'Start year', 4)}
          <span className="hint">(MM/DD/YYYY)</span>
        </fieldset>
        <fieldset className="field date-group">
          <legend>End Date :</legend>
          {dateInput('edtMm', 'End month', 2)} / {dateInput('edtDd', 'End day', 2)} / {dateInput('edtYyyy', 'End year', 4)}
          <span className="hint">(MM/DD/YYYY)</span>
        </fieldset>
      </div>
      <div className="form-grid confirm-row">
        <Field
          id="rp-confirm"
          label="The Report will be submitted for printing. Please confirm:"
          value={form.confirm}
          onChange={set('confirm')}
          maxLength={1}
          hint="(Y/N)"
          upper
          invalid={invalid === 'confirm'}
        />
      </div>
    </Screen>
  );
}
