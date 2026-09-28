import { useCallback, useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { api } from '../api/endpoints';
import type { PageQuery, UserSummary } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { PAGE_SIZE } from '../config';
import { useBack } from '../lib/navigation';
import { inclusiveStartKey, usePager } from '../lib/usePager';
import { blank } from '../validation/common';

const PAGER_MESSAGES = {
  top: 'You are already at the top of the page...',
  bottom: 'You are already at the bottom of the page...',
};

export function UserListScreen() {
  const back = useBack('/admin');
  const navigate = useNavigate();
  const [search, setSearch] = useState('');
  const [selections, setSelections] = useState<Record<string, string>>({});
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const fetchPage = useCallback((q: PageQuery) => api.listUsers(q), []);
  const pager = usePager<UserSummary>(fetchPage, PAGE_SIZE.users, PAGER_MESSAGES);
  const { load } = pager;

  useEffect(() => {
    void load();
  }, [load]);

  const rows = pager.page?.items ?? [];

  const enter = () => {
    const picked = Object.entries(selections).filter(([, v]) => v.trim() !== '');
    if (picked.length > 0) {
      const [userId, code] = picked[0];
      const c = code.trim().toUpperCase();
      if (c !== 'U' && c !== 'D') return setMessage({ text: 'Invalid selection. Valid values are U and D', kind: 'error' });
      navigate(`/admin/users/${encodeURIComponent(userId)}/${c === 'U' ? 'edit' : 'delete'}`, {
        state: { from: '/admin/users' },
      });
      return;
    }
    setMessage(null);
    setSelections({});
    void load(blank(search) ? undefined : inclusiveStartKey(search.toUpperCase()));
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
      tranId="CU00"
      program="COUSR00C"
      title="List Users"
      headerExtra={<span className="page-no">Page: {pager.pageNum || 1}</span>}
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
        <Field id="usr-search" label="Search User ID:" value={search} onChange={setSearch} maxLength={8} upper autoFocus />
      </div>
      <table className="grid" aria-label="Users">
        <thead>
          <tr>
            <th scope="col">Sel</th>
            <th scope="col">User ID</th>
            <th scope="col">First Name</th>
            <th scope="col">Last Name</th>
            <th scope="col">Type</th>
          </tr>
        </thead>
        <tbody>
          {Array.from({ length: PAGE_SIZE.users }, (_, i) => rows[i]).map((row, i) => (
            <tr key={row?.userId ?? `empty-${i}`} className={row ? '' : 'empty-row'}>
              <td>
                {row && (
                  <input
                    className="sel"
                    aria-label={`Select user ${row.userId}`}
                    value={selections[row.userId] ?? ''}
                    maxLength={1}
                    onChange={(e) => setSelections((s) => ({ ...s, [row.userId]: e.target.value.toUpperCase() }))}
                  />
                )}
              </td>
              <td>{row?.userId ?? ''}</td>
              <td>{row?.firstName ?? ''}</td>
              <td>{row?.lastName ?? ''}</td>
              <td>{row?.userType ?? ''}</td>
            </tr>
          ))}
        </tbody>
      </table>
      <p className="hint-line">Type &apos;U&apos; to Update or &apos;D&apos; to Delete a User from the list</p>
    </Screen>
  );
}
