import { useCallback, useState } from 'react';
import { errorMessage } from '../api/client';
import type { Direction, Page, PageQuery } from '../api/types';

export interface PagerMessages {
  top: string;
  bottom: string;
}

export interface Pager<T> {
  page: Page<T> | null;
  pageNum: number;
  busy: boolean;
  error: string | null;
  notice: string | null;
  load: (startKey?: string) => Promise<void>;
  next: () => Promise<void>;
  prev: () => Promise<void>;
}

/** Keyset paging (`fetchPage` must be memoized; a new identity restarts nothing by itself).
 * (api.md §2): PF8 -> direction=next from lastKey, PF7 -> direction=prev from firstKey. */
export function usePager<T>(fetchPage: (q: PageQuery) => Promise<Page<T>>, pageSize: number, messages: PagerMessages): Pager<T> {
  const [page, setPage] = useState<Page<T> | null>(null);
  const [pageNum, setPageNum] = useState(0);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [notice, setNotice] = useState<string | null>(null);

  const run = useCallback(
    async (query: PageQuery, nextNum: number) => {
      setBusy(true);
      setError(null);
      setNotice(null);
      try {
        const result = await fetchPage({ pageSize, ...query });
        setPage(result);
        setPageNum(nextNum);
      } catch (err) {
        setError(errorMessage(err));
      } finally {
        setBusy(false);
      }
    },
    [fetchPage, pageSize],
  );

  const load = useCallback((startKey?: string) => run({ direction: 'next', startKey }, 1), [run]);

  const move = useCallback(
    async (direction: Direction) => {
      if (!page) return;
      const can = direction === 'next' ? page.hasNext : page.hasPrev;
      if (!can) {
        setError(null);
        setNotice(direction === 'next' ? messages.bottom : messages.top);
        return;
      }
      const startKey = direction === 'next' ? page.lastKey : page.firstKey;
      await run({ startKey: startKey ?? undefined, direction }, Math.max(1, pageNum + (direction === 'next' ? 1 : -1)));
    },
    [page, pageNum, run, messages.bottom, messages.top],
  );

  return {
    page,
    pageNum,
    busy,
    error,
    notice,
    load,
    next: () => move('next'),
    prev: () => move('prev'),
  };
}

/**
 * Largest key strictly below `key`, so that `direction=next` (rows `> startKey`) starts at `key` itself —
 * the legacy "search" field positions the browse with `STARTBR ... GTEQ`.
 */
export function inclusiveStartKey(key: string, numericWidth?: number): string | undefined {
  const k = key.trim();
  if (!k) return undefined;
  if (numericWidth) {
    const n = BigInt(k);
    return n === 0n ? undefined : (n - 1n).toString().padStart(numericWidth, '0');
  }
  const last = k.charCodeAt(k.length - 1);
  return `${k.slice(0, -1)}${String.fromCharCode(last - 1)}~~~~~~~~`;
}
