import { useCallback, useState } from 'react';
import { ApiError, errorText } from '../api/client';
import type { MessageKind, ScreenMessage } from '../components/Screen';
import { focusField } from './focus';

/** ERRMSG area + the fields the API flagged (ApiError.field / invalidFields). */
export function useMessages() {
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<string[]>([]);

  const fail = useCallback((err: unknown) => {
    setMessage({ text: errorText(err), kind: 'error' });
    if (err instanceof ApiError) {
      const fields = err.problem.invalidFields?.length ? err.problem.invalidFields : err.field ? [err.field] : [];
      setInvalid(fields);
      focusField(err.field);
    } else {
      setInvalid([]);
    }
  }, []);

  const say = useCallback((text: string | null | undefined, kind: MessageKind = 'info') => {
    setInvalid([]);
    setMessage(text && text.trim() ? { text, kind } : null);
  }, []);

  const clear = useCallback(() => {
    setInvalid([]);
    setMessage(null);
  }, []);

  const isInvalid = useCallback(
    (id: string) => invalid.some((f) => id === f || id.startsWith(f.endsWith('.') ? f : `${f}.`)),
    [invalid],
  );

  return { message, fail, say, clear, isInvalid };
}
