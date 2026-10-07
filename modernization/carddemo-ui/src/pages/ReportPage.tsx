import { useEffect, useRef, useState } from 'react';
import { api } from '../api/endpoints';
import type { NavigationContext, ReportDateFields, ReportExecutionResponse, ScreenHeader } from '../api/types';
import { Field, FieldGroup, Part } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('CORPT00C');
const TYPES = [
  { value: 'Monthly', bms: 'MONTHLY', label: 'Monthly (Current Month)' },
  { value: 'Yearly', bms: 'YEARLY', label: 'Yearly (Current Year)' },
  { value: 'Custom', bms: 'CUSTOM', label: 'Custom (Date Range)' },
] as const;
const BLANK: ReportDateFields = { month: '', day: '', year: '' };
export const POLL_MS = 1000;

/** CORPT00 / CORPT0A — CORPT00C (CR00). Submit (202 + executionId), poll, download TRANREPT (ADR-0021). */
export function ReportPage() {
  const { back } = useProgramNav();
  const msg = useMessages();
  const [reportType, setReportType] = useState('');
  const [startDate, setStartDate] = useState<ReportDateFields>(BLANK);
  const [endDate, setEndDate] = useState<ReportDateFields>(BLANK);
  const [confirm, setConfirm] = useState('');
  const [header, setHeader] = useState<ScreenHeader | null>(null);
  const [exit, setExit] = useState<NavigationContext | null>(null);
  const [executionId, setExecutionId] = useState<number | null>(null);
  const [execution, setExecution] = useState<ReportExecutionResponse | null>(null);
  const [busy, setBusy] = useState(false);
  const timer = useRef<number | null>(null);

  useEffect(() => {
    if (executionId === null) return;
    let live = true;
    const poll = async () => {
      try {
        const status = await api.reportExecution(executionId);
        if (!live) return;
        setExecution(status);
        if (status.status === 'QUEUED' || status.status === 'RUNNING') {
          timer.current = window.setTimeout(poll, POLL_MS);
        }
      } catch (err) {
        if (live) msg.fail(err);
      }
    };
    void poll();
    return () => {
      live = false;
      if (timer.current !== null) window.clearTimeout(timer.current);
    };
  }, [executionId, msg.fail]); // eslint-disable-line react-hooks/exhaustive-deps

  const enter = async () => {
    setBusy(true);
    try {
      const custom = reportType === 'Custom';
      const result = await api.submitReport({
        reportType,
        startDate: custom ? startDate : BLANK,
        endDate: custom ? endDate : BLANK,
        confirm,
      });
      setHeader(result.header);
      setExit(result.exit);
      msg.say(result.message, result.state === 'SUBMITTED' ? 'success' : 'info');
      if (result.state === 'SUBMITTED' && result.executionId !== null) {
        setExecution(null);
        setExecutionId(result.executionId);
        setConfirm('');
      }
      if (result.state === 'CANCELLED') setConfirm('');
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  const save = (blob: Blob, name: string) => {
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = name;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  };

  /** Text version: the report lines the API already decoded in the execution status. */
  const downloadText = () => {
    const lines = execution?.report?.lines;
    if (executionId === null || !lines) return;
    save(new Blob([`${lines.join('\n')}\n`], { type: 'text/plain;charset=utf-8' }), `TRANREPT-${executionId}.txt`);
  };

  /** Catalogued TRANREPT bytes unchanged (ADR-0021: EBCDIC by default, same as the batch CLI). */
  const downloadRaw = async () => {
    if (executionId === null) return;
    try {
      save(await api.reportFile(executionId), `TRANREPT-${executionId}.${(execution?.report?.encoding ?? 'raw').toLowerCase().replace(/[^a-z0-9-]/g, '')}`);
    } catch (err) {
      msg.fail(err);
    }
  };

  const date = (base: 'startDate' | 'endDate', value: ReportDateFields, set: (d: ReportDateFields) => void, bms: string, label: string) => (
    <FieldGroup label={label} htmlFor={`${base}.month`}>
      <Part id={`${base}.month`} bms={`${bms}MM`} label={`${label} month`} value={value.month} onChange={(v) => set({ ...value, month: v })} maxLength={2} numeric invalid={msg.isInvalid(`${base}.month`)} />
      <span>/</span>
      <Part id={`${base}.day`} bms={`${bms}DD`} label={`${label} day`} value={value.day} onChange={(v) => set({ ...value, day: v })} maxLength={2} numeric invalid={msg.isInvalid(`${base}.day`)} />
      <span>/</span>
      <Part id={`${base}.year`} bms={`${bms}YYYY`} label={`${label} year`} value={value.year} onChange={(v) => set({ ...value, year: v })} maxLength={4} numeric invalid={msg.isInvalid(`${base}.year`)} />
      <span className="hint">(MM/DD/YYYY)</span>
    </FieldGroup>
  );

  return (
    <Screen
      page={PAGE}
      header={header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => void enter() },
        { key: 'F3', label: 'Back', action: () => back(PAGE, exit) },
      ]}
    >
      <fieldset className="choices" id="reportType">
        <legend>Select a report type to print</legend>
        {TYPES.map((t) => (
          <label key={t.value} className="choice">
            <input
              type="radio"
              name="reportType"
              id={`reportType.${t.value}`}
              data-bms={t.bms}
              value={t.value}
              checked={reportType === t.value}
              onChange={() => setReportType(t.value)}
            />
            {t.label}
          </label>
        ))}
      </fieldset>
      <div className="form-grid narrow">
        {date('startDate', startDate, setStartDate, 'SDT', 'Start Date')}
        {date('endDate', endDate, setEndDate, 'EDT', 'End Date')}
        <Field id="confirm" bms="CONFIRM" label="The Report will be submitted for printing. Please confirm" value={confirm} onChange={setConfirm} maxLength={1} upper hint="(Y/N)" invalid={msg.isInvalid('confirm')} />
      </div>
      {executionId !== null && (
        <section className="report-status" aria-label="Report execution">
          <div>
            Execution <strong>{executionId}</strong>: <span data-testid="report-status">{execution?.status ?? 'QUEUED'}</span>
            {execution?.returnCode !== null && execution?.returnCode !== undefined && <> (RC {execution.returnCode})</>}
          </div>
          {execution?.message && <div>{execution.message}</div>}
          {execution?.status === 'COMPLETED' && (
            <>
              {execution.report?.lines?.length ? (
                <button type="button" className="action" data-testid="download-text" onClick={downloadText}>
                  Download TRANREPT (text)
                </button>
              ) : null}
              <button type="button" className="action" data-testid="download-raw" onClick={() => void downloadRaw()}>
                Download as catalogued (raw bytes)
              </button>
              {execution.report?.lines?.length ? (
                <pre className="report-preview" data-testid="report-preview">
                  {execution.report.lines.slice(0, 40).join('\n')}
                </pre>
              ) : null}
            </>
          )}
        </section>
      )}
    </Screen>
  );
}
