import { useEffect } from 'react';

/** Runs `run` once after mount (outside the effect body), e.g. a lookup for a key handed over by the caller. */
export function useMountEffect(run: () => void): void {
  useEffect(() => {
    const id = window.setTimeout(run, 0);
    return () => window.clearTimeout(id);
    // intentionally mount-only
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);
}
