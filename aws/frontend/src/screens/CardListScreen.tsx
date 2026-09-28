import { useCallback, useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { api } from '../api/endpoints';
import type { CardSummary, PageQuery } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { PAGE_SIZE } from '../config';
import { useBack } from '../lib/navigation';
import { usePager } from '../lib/usePager';
import { validateCardFilters } from '../validation/card';

const PAGER_MESSAGES = { top: 'NO PREVIOUS PAGES TO DISPLAY', bottom: 'NO MORE PAGES TO DISPLAY' };

export function CardListScreen() {
  const back = useBack();
  const navigate = useNavigate();
  const [acctId, setAcctId] = useState('');
  const [cardNum, setCardNum] = useState('');
  const [filters, setFilters] = useState<{ acctId?: string; cardNum?: string }>({});
  const [selections, setSelections] = useState<Record<string, string>>({});
  const [message, setMessage] = useState<ScreenMessage | null>(null);

  const fetchPage = useCallback((q: PageQuery) => api.listCards(filters, q), [filters]);
  const pager = usePager<CardSummary>(fetchPage, PAGE_SIZE.cards, PAGER_MESSAGES);
  const { load } = pager;

  useEffect(() => {
    void load();
  }, [load, filters]);

  const rows = pager.page?.items ?? [];

  const enter = () => {
    const filterError = validateCardFilters(acctId, cardNum);
    if (filterError) return setMessage({ text: filterError.message, kind: 'error' });

    const picked = Object.entries(selections).filter(([, v]) => v.trim() !== '');
    if (picked.length > 1) return setMessage({ text: 'PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE', kind: 'error' });
    if (picked.length === 1) {
      const [num, action] = picked[0];
      const card = rows.find((r) => r.cardNum === num);
      const code = action.trim().toUpperCase();
      if (!card || !['S', 'U'].includes(code)) return setMessage({ text: 'INVALID ACTION CODE', kind: 'error' });
      const target = code === 'S' ? '/cards/view' : '/cards/update';
      navigate(target, {
        state: { from: '/cards', acctId: String(card.acctId).padStart(11, '0'), cardNum: card.cardNum },
      });
      return;
    }
    setMessage(null);
    setSelections({});
    setFilters({ acctId: acctId || undefined, cardNum: cardNum || undefined });
  };

  const page = async (fn: () => Promise<void>) => {
    setSelections({});
    setMessage(null);
    await fn();
  };

  const shownMessage: ScreenMessage | null =
    message ??
    (pager.error ? { text: pager.error, kind: 'error' } : null) ??
    (pager.notice ? { text: pager.notice, kind: 'info' } : null) ??
    (pager.page && rows.length === 0 ? { text: 'NO RECORDS FOUND FOR THIS SEARCH CONDITION.', kind: 'info' } : null);

  return (
    <Screen
      tranId="CCLI"
      program="COCRDLIC"
      title="List Credit Cards"
      headerExtra={<span className="page-no">Page {pager.pageNum || 1}</span>}
      message={shownMessage}
      busy={pager.busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: enter },
        { key: 'F3', label: 'Exit', action: back },
        { key: 'F7', label: 'Backward', action: () => page(pager.prev) },
        { key: 'F8', label: 'Forward', action: () => page(pager.next) },
      ]}
    >
      <div className="form-grid">
        <Field
          id="cc-acct-filter"
          label="Account Number :"
          value={acctId}
          onChange={(v) => setAcctId(v.replace(/\D/g, ''))}
          maxLength={11}
          inputMode="numeric"
          autoFocus
        />
        <Field
          id="cc-card-filter"
          label="Credit Card Number :"
          value={cardNum}
          onChange={(v) => setCardNum(v.replace(/\D/g, ''))}
          maxLength={16}
          inputMode="numeric"
        />
      </div>
      <table className="grid" aria-label="Credit cards">
        <thead>
          <tr>
            <th scope="col">Select</th>
            <th scope="col">Account Number</th>
            <th scope="col">Card Number</th>
            <th scope="col">Active</th>
          </tr>
        </thead>
        <tbody>
          {Array.from({ length: PAGE_SIZE.cards }, (_, i) => rows[i]).map((row, i) => (
            <tr key={row?.cardNum ?? `empty-${i}`} className={row ? '' : 'empty-row'}>
              <td>
                {row && (
                  <input
                    className="sel"
                    aria-label={`Select card ${row.cardNum}`}
                    value={selections[row.cardNum] ?? ''}
                    maxLength={1}
                    onChange={(e) => setSelections((s) => ({ ...s, [row.cardNum]: e.target.value.toUpperCase() }))}
                  />
                )}
              </td>
              <td>{row ? String(row.acctId).padStart(11, '0') : ''}</td>
              <td>{row?.cardNum ?? ''}</td>
              <td>{row?.activeStatus ?? ''}</td>
            </tr>
          ))}
        </tbody>
      </table>
      <p className="hint-line">Type &apos;S&apos; to View or &apos;U&apos; to Update a card, then press ENTER.</p>
    </Screen>
  );
}
