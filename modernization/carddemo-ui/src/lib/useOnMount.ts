import { useEffect, useRef } from 'react';

/** Runs `run` once after mount, outside the effect body (e.g. the lookup for a key handed over by the caller). */
export function useOnMount(run: () => void): void {
  const ref = useRef(run);
  useEffect(() => {
    ref.current = run;
  });
  useEffect(() => {
    const id = window.setTimeout(() => ref.current(), 0);
    return () => window.clearTimeout(id);
  }, []);
}
