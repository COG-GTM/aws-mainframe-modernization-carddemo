import { useEffect, useRef, useState, type FormEvent, type ReactNode } from 'react';
import type { ScreenHeader } from '../api/types';
import type { ProgramPage } from '../programs';

export type PfKeyName = 'ENTER' | 'F3' | 'F4' | 'F5' | 'F7' | 'F8' | 'F12';

export interface PfKey {
  key: PfKeyName;
  label: string;
  action: () => void;
  disabled?: boolean;
}

export type MessageKind = 'error' | 'info' | 'success';

export interface ScreenMessage {
  text: string;
  kind: MessageKind;
}

export interface ScreenProps {
  page: ProgramPage;
  header?: ScreenHeader | null;
  message?: ScreenMessage | null;
  info?: string | null;
  pfKeys: PfKey[];
  children?: ReactNode;
  busy?: boolean;
  titleExtra?: ReactNode;
}

const pad = (n: number) => String(n).padStart(2, '0');
const legacyDate = (d: Date) => `${pad(d.getMonth() + 1)}/${pad(d.getDate())}/${pad(d.getFullYear() % 100)}`;
const legacyTime = (d: Date) => `${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;

const FKEY = /^F(\d{1,2})$/;

/** Common layout of a BMS map: ScreenHeader, body, INFOMSG, ERRMSG and the PF-key line as buttons. */
export function Screen({ page, header, message, info, pfKeys, children, busy, titleExtra }: ScreenProps) {
  const [now, setNow] = useState(() => new Date());
  const keysRef = useRef(pfKeys);
  const busyRef = useRef(busy);

  useEffect(() => {
    keysRef.current = pfKeys;
    busyRef.current = busy;
  });

  useEffect(() => {
    const id = window.setInterval(() => setNow(new Date()), 1000);
    return () => window.clearInterval(id);
  }, []);

  useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      const m = FKEY.exec(event.key);
      const name = m ? (`F${m[1]}` as PfKeyName) : event.key === 'Escape' ? 'F3' : null;
      const pf = name ? keysRef.current.find((k) => k.key === name) : undefined;
      if (!pf) return;
      event.preventDefault();
      if (!pf.disabled && !busyRef.current) pf.action();
    };
    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, []);

  const enter = pfKeys.find((k) => k.key === 'ENTER');
  const onSubmit = (event: FormEvent) => {
    event.preventDefault();
    if (enter && !enter.disabled && !busy) enter.action();
  };

  return (
    <div className="screen" data-program={page.program} data-map={page.mapset}>
      <header className="screen-header">
        <div className="hdr-left">
          <div>
            <span className="hdr-label">Tran:</span> <span data-testid="tran-id">{page.tranId}</span>
          </div>
          <div>
            <span className="hdr-label">Prog:</span> <span data-testid="program">{page.program}</span>
          </div>
        </div>
        <div className="hdr-center">
          <div className="title01">{header?.title01 || 'AWS Mainframe Modernization'}</div>
          <div className="title02">{header?.title02 || 'CardDemo'}</div>
        </div>
        <div className="hdr-right">
          <div>
            <span className="hdr-label">Date:</span> {legacyDate(now)}
          </div>
          <div>
            <span className="hdr-label">Time:</span> {legacyTime(now)}
          </div>
        </div>
      </header>
      <div className="screen-title-row">
        <h1 className="screen-title">{page.title}</h1>
        <span className="screen-map">
          {page.mapset}/{page.map}
        </span>
        {titleExtra}
      </div>
      <form className="screen-body" onSubmit={onSubmit} noValidate aria-busy={busy || undefined}>
        {children}
        <button type="submit" hidden aria-hidden="true" tabIndex={-1} />
      </form>
      <div className="info-line" role="status" aria-live="polite" data-testid="info-line">
        {info ?? ''}
      </div>
      <div
        className={`message-line ${message ? `message-${message.kind}` : ''}`}
        role={message?.kind === 'error' ? 'alert' : 'status'}
        data-testid="message-line"
      >
        {message?.text ?? ''}
      </div>
      <nav className="pf-bar" aria-label="Function keys">
        {pfKeys.map((pf) => (
          <button
            key={pf.key}
            type="button"
            className={`pf-key pf-${pf.key.toLowerCase()}`}
            onClick={() => pf.action()}
            disabled={pf.disabled || busy}
            aria-keyshortcuts={pf.key === 'ENTER' ? 'Enter' : pf.key}
          >
            <kbd>{pf.key}</kbd> {pf.label}
          </button>
        ))}
      </nav>
    </div>
  );
}
