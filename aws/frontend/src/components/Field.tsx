import { forwardRef, type InputHTMLAttributes, type ReactNode } from 'react';

export interface FieldProps extends Omit<InputHTMLAttributes<HTMLInputElement>, 'onChange' | 'value'> {
  label: string;
  value: string;
  onChange?: (value: string) => void;
  hint?: ReactNode;
  invalid?: boolean;
  width?: number;
  upper?: boolean;
}

export const Field = forwardRef<HTMLInputElement, FieldProps>(function Field(
  { label, value, onChange, hint, invalid, width, upper, id, readOnly, maxLength, className, ...rest },
  ref,
) {
  const inputId = id ?? `f-${label.replace(/[^a-z0-9]+/gi, '-').toLowerCase()}`;
  const chars = width ?? maxLength ?? 20;
  return (
    <div className={`field ${readOnly ? 'field-ro' : ''} ${className ?? ''}`}>
      <label htmlFor={inputId}>{label}</label>
      <input
        ref={ref}
        id={inputId}
        value={value}
        readOnly={readOnly}
        maxLength={maxLength}
        aria-invalid={invalid || undefined}
        style={{ width: `calc(${Math.min(chars, 52)}ch + 1.4rem)` }}
        onChange={(e) => onChange?.(upper ? e.target.value.toUpperCase() : e.target.value)}
        autoComplete="off"
        spellCheck={false}
        {...rest}
      />
      {hint && <span className="hint">{hint}</span>}
    </div>
  );
});

export function ReadOnly({ label, value, width }: { label: string; value: ReactNode; width?: number }) {
  return (
    <div className="field field-ro">
      <span className="label">{label}</span>
      <output style={width ? { minWidth: `calc(${width}ch + 1.4rem)` } : undefined}>{value}</output>
    </div>
  );
}
