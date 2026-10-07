import type { InputHTMLAttributes, ReactNode } from 'react';

export interface FieldProps extends Omit<InputHTMLAttributes<HTMLInputElement>, 'onChange' | 'value' | 'id'> {
  /** API field path (ApiError.field), used as the DOM id. */
  id: string;
  /** BMS symbolic field name (e.g. ACCTSID), shown as a data attribute for traceability. */
  bms: string;
  label: string;
  value: string;
  onChange?: (value: string) => void;
  hint?: ReactNode;
  invalid?: boolean;
  /** BMS NUM attribute: digits only. */
  numeric?: boolean;
  upper?: boolean;
  /** DRK attribute: not echoed. */
  dark?: boolean;
}

/** One BMS input/output field; LENGTH becomes maxLength, NUM / DRK become numeric / password input. */
export function Field({ id, bms, label, value, onChange, hint, invalid, numeric, upper, dark, readOnly, maxLength, className, ...rest }: FieldProps) {
  const chars = maxLength ?? 20;
  return (
    <div className={`field ${readOnly ? 'field-ro' : ''} ${className ?? ''}`}>
      <label htmlFor={id}>{label}</label>
      <input
        id={id}
        name={id}
        data-bms={bms}
        value={value ?? ''}
        readOnly={readOnly}
        tabIndex={readOnly ? -1 : undefined}
        maxLength={maxLength}
        type={dark ? 'password' : 'text'}
        inputMode={numeric ? 'numeric' : undefined}
        aria-invalid={invalid || undefined}
        style={{ width: `calc(${Math.min(chars, 52)}ch + 1.4rem)` }}
        onChange={(e) => {
          let next = e.target.value;
          if (numeric) next = next.replace(/\D/g, '');
          if (upper) next = next.toUpperCase();
          onChange?.(next);
        }}
        autoComplete="off"
        spellCheck={false}
        {...rest}
      />
      {hint && <span className="hint">{hint}</span>}
    </div>
  );
}

/** A row of fields sharing one label (dates, SSN, phone parts). */
export function FieldGroup({ label, children, htmlFor }: { label: string; children: ReactNode; htmlFor?: string }) {
  return (
    <div className="field field-group">
      <label htmlFor={htmlFor}>{label}</label>
      <div className="group-parts">{children}</div>
    </div>
  );
}

export function Part({ id, bms, value, onChange, maxLength, readOnly, invalid, label, numeric }: {
  id: string;
  bms: string;
  value: string;
  onChange?: (v: string) => void;
  maxLength: number;
  readOnly?: boolean;
  invalid?: boolean;
  label: string;
  numeric?: boolean;
}) {
  return (
    <input
      id={id}
      name={id}
      data-bms={bms}
      aria-label={label}
      value={value ?? ''}
      maxLength={maxLength}
      readOnly={readOnly}
      inputMode={numeric ? 'numeric' : undefined}
      aria-invalid={invalid || undefined}
      style={{ width: `calc(${maxLength}ch + 1.4rem)` }}
      onChange={(e) => onChange?.(numeric ? e.target.value.replace(/\D/g, '') : e.target.value)}
      autoComplete="off"
      spellCheck={false}
    />
  );
}
