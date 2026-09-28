import { useCallback, useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { api } from '../api/endpoints';
import type { PageQuery, TransactionSummary } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { PAGE_SIZE } from '../config';
import { shortDate } from '../lib/format';
import { useBack } from '../lib/navigation';
import { inclusiveStartKey, usePager } from '../lib/usePager';
import { blank, isDigits } from '../validation/common';

const TRAN_PAGER_MESSAGES = {
  top: 'You are already at the top of the page...',
  bottom: 'You are already at the bottom of the page...',
};

const amountDisplay = (amt: string) => {
  const n = Number(amt);
  return `${n < 0 ? '-' : '+'}${Math.abs(n).toFixed(2).padStart(11, '0')}`;
};

export function TransactionListScreen() {
  const back = useBack();
  const navigate = useNavigate();
  const [search, setSearch] = useState('');
  const [selections, setSelections] = useState<Record<string, string>>({});
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const fetchPage = useCallback((q: PageQuery) => api.listTransactions(q), []);
  const pager = usePager<TransactionSummary>(fetchPage, PAGE_SIZE.transactions, TRAN_PAGER_MESSAGES);
  const { load } = pager;

  useEffect(() => {
    void load();
  }, [load]);

  const rows = pager.page?.items ?? [];

  const enter = () => {
    const picked = Object.entries(selections).filter(([, v]) => v.trim() !== '');
    if (picked.length > 0) {
      const [tranId, code] = picked[0];
      if (code.trim().toUpperCase() !== 'S') {
        return setMessage({ text: 'Invalid selection. Valid value is S', kind: 'error' });
      }
      navigate('/transactions/view', { state: { from: '/transactions', tranId } });
      return;
    }
    if (!blank(search) && !isDigits(search)) return setMessage({ text: 'Tran ID must be Numeric ...', kind: 'error' });
    setMessage(null);
    setSelections({});
    void load(blank(search) ? undefined : inclusiveStartKey(search, 16));
  };

  const page = async (fn: () => Promise<void>) => {
    setSelections({});
    setMessage(null);
    await fn();
  };

  const shownMessage: ScreenMessage | null =
    message ??
    (pager.error ? { text: pager.error, kind: 'error' } : null) ??
    (pager.notice ? { text: pager.notice, kind: 'info' } : null);

  return (
    <Screen
      tranId="CT00"
      program="COTRN00C"
      title="List Transactions"
      headerExtra={<span className="page-no" data-testid="page-no">Page: {pager.pageNum || 1}</span>}
      message={shownMessage}
      busy={pager.busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: enter },
        { key: 'F3', label: 'Back', action: back },
        { key: 'F7', label: 'Backward', action: () => page(pager.prev) },
        { key: 'F8', label: 'Forward', action: () => page(pager.next) },
      ]}
    >
      <div className="form-grid">
        <Field id="tran-search" label="Search Tran ID:" value={search} onChange={setSearch} maxLength={16} autoFocus />
      </div>
      <table className="grid" aria-label="Transactions">
        <thead>
          <tr>
            <th scope="col">Sel</th>
            <th scope="col">Transaction ID</th>
            <th scope="col">Date</th>
            <th scope="col">Description</th>
            <th scope="col" className="num">
              Amount
            </th>
          </tr>
        </thead>
        <tbody>
          {Array.from({ length: PAGE_SIZE.transactions }, (_, i) => rows[i]).map((row, i) => (
            <tr key={row?.tranId ?? `empty-${i}`} className={row ? '' : 'empty-row'}>
              <td>
                {row && (
                  <input
                    className="sel"
                    aria-label={`Select transaction ${row.tranId}`}
                    value={selections[row.tranId] ?? ''}
                    maxLength={1}
                    onChange={(e) => setSelections((s) => ({ ...s, [row.tranId]: e.target.value.toUpperCase() }))}
                  />
                )}
              </td>
              <td>{row?.tranId ?? ''}</td>
              <td>{row ? shortDate(row.origDate) : ''}</td>
              <td>{row?.description.slice(0, 26) ?? ''}</td>
              <td className="num">{row ? amountDisplay(row.amt) : ''}</td>
            </tr>
          ))}
        </tbody>
      </table>
      <p className="hint-line">Type &apos;S&apos; to View Transaction details from the list</p>
    </Screen>
  );
}
