import { useCallback, useRef } from 'react';

/** Cursor positioning (BMS `MOVE -1 TO <field>L`): focus an input by id after render. */
export function useFieldFocus(): (id: string) => void {
  const pending = useRef<number | null>(null);
  return useCallback((id: string) => {
    if (pending.current !== null) window.cancelAnimationFrame(pending.current);
    pending.current = window.requestAnimationFrame(() => {
      const el = document.getElementById(id);
      if (el instanceof HTMLInputElement || el instanceof HTMLSelectElement) el.focus();
    });
  }, []);
}
