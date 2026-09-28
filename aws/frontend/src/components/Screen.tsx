import { useEffect, useRef, useState, type FormEvent, type ReactNode } from 'react';
import { TITLE01, TITLE02 } from '../config';
import { legacyDate, legacyTime } from '../lib/format';

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
  tranId: string;
  program: string;
  title?: string;
  message?: ScreenMessage | null;
  info?: string | null;
  pfKeys: PfKey[];
  children: ReactNode;
  busy?: boolean;
  headerExtra?: ReactNode;
}

function useClock(): Date {
  const [now, setNow] = useState(() => new Date());
  useEffect(() => {
    const id = window.setInterval(() => setNow(new Date()), 1000);
    return () => window.clearInterval(id);
  }, []);
  return now;
}

export function Screen({ tranId, program, title, message, info, pfKeys, children, busy, headerExtra }: ScreenProps) {
  const now = useClock();
  const keysRef = useRef(pfKeys);
  const busyRef = useRef(busy);

  useEffect(() => {
    keysRef.current = pfKeys;
    busyRef.current = busy;
  });

  useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      const name = event.key === 'Escape' ? 'F12' : event.key;
      if (!/^F\d{1,2}$/.test(name)) return;
      const pf =
        keysRef.current.find((k) => k.key === name) ??
        (event.key === 'Escape' ? keysRef.current.find((k) => k.key === 'F3') : undefined);
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
    <div className="screen" data-program={program}>
      <header className="screen-header">
        <div className="hdr-left">
          <div>
            <span className="hdr-label">Tran:</span> <span data-testid="tran-id">{tranId}</span>
          </div>
          <div>
            <span className="hdr-label">Prog:</span> <span data-testid="program">{program}</span>
          </div>
        </div>
        <div className="hdr-center">
          <div className="title01">{TITLE01}</div>
          <div className="title02">{TITLE02}</div>
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
      {(title || headerExtra) && (
        <div className="screen-title-row">
          {title && <h1 className="screen-title">{title}</h1>}
          {headerExtra}
        </div>
      )}
      <form className="screen-body" onSubmit={onSubmit} noValidate aria-busy={busy || undefined}>
        {children}
        <button type="submit" hidden aria-hidden="true" tabIndex={-1} />
      </form>
      <div className="info-line" role="status" aria-live="polite">
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
            <kbd>{pf.key === 'ENTER' ? 'ENTER' : pf.key}</kbd>={pf.label}
          </button>
        ))}
      </nav>
    </div>
  );
}
