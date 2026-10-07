import { useState } from 'react';
import { api } from '../api/endpoints';
import type { UserListScreen } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COUSR00C');
const ROWS = 10;

/** COUSR00 / COUSR0A — COUSR00C (CU00). U = update, D = delete. */
export function UserListPage() {
  const { back, go } = useProgramNav();
  const msg = useMessages();
  const [startUserId, setStartUserId] = useState('');
  const [list, setList] = useState<UserListScreen | null>(null);
  const [selections, setSelections] = useState<string[]>([]);
  const [busy, setBusy] = useState(false);

  const load = async (q: { startUserId?: string; after?: string | null; before?: string | null; page?: number } = {}) => {
    setBusy(true);
    try {
      const result = await api.users(q);
      if ((q.after || q.before) && result.rows.length === 0 && list?.rows.length) {
        // Past the first/last page: keep the rows on screen and show the program's message.
        setList({ ...list, hasNextPage: q.after ? false : list.hasNextPage, hasPreviousPage: q.before ? false : list.hasPreviousPage });
      } else {
        setList(result);
        setSelections(result.rows.map(() => ''));
      }
      msg.say(result.message);
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    void load();
  });

  const rows = list?.rows ?? [];

  const enter = async () => {
    if (selections.some((s) => s.trim())) {
      setBusy(true);
      try {
        const result = await api.selectUser(rows.map((r, i) => ({ userId: r.userId, selection: selections[i] ?? '' })));
        go(result.navigation, { userId: result.userId });
      } catch (err) {
        msg.fail(err);
      } finally {
        setBusy(false);
      }
      return;
    }
    await load({ startUserId: startUserId.trim() });
  };

  const forward = () => load({ after: list?.nextPage ?? rows[rows.length - 1]?.userId, page: list?.pageNumber });
  const backward = () => load({ before: list?.previousPage ?? rows[0]?.userId, page: list?.pageNumber });

  return (
    <Screen
      page={PAGE}
      header={list?.header}
      message={msg.message}
      busy={busy}
      info="Type 'U' to Update or 'D' to Delete a User from the list"
      titleExtra={
        <span className="page-no">
          Page: <span data-bms="PAGENUM">{list?.pageNumber ?? ''}</span>
        </span>
      }
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => void enter() },
        { key: 'F3', label: 'Back', action: () => back(PAGE, list?.exit) },
        { key: 'F7', label: 'Backward', action: () => void backward() },
        { key: 'F8', label: 'Forward', action: () => void forward() },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="startUserId" bms="USRIDIN" label="Search User ID" value={startUserId} onChange={setStartUserId} maxLength={8} upper autoFocus invalid={msg.isInvalid('startUserId')} />
      </div>
      <table className="grid">
        <thead>
          <tr>
            <th>Sel</th>
            <th>User ID</th>
            <th>First Name</th>
            <th>Last Name</th>
            <th>Type</th>
          </tr>
        </thead>
        <tbody>
          {Array.from({ length: ROWS }, (_, i) => {
            const row = rows[i];
            const n = String(i + 1).padStart(2, '0');
            return (
              <tr key={i} data-testid={`user-row-${i + 1}`}>
                <td>
                  <input
                    disabled={!row}
                    id={`rows[${i}].selection`}
                    data-bms={`SEL00${n}`}
                    aria-label={row ? `Select ${row.userId}` : `Select row ${i + 1}`}
                    className="sel"
                    maxLength={1}
                    value={selections[i] ?? ''}
                    aria-invalid={msg.isInvalid(`rows[${i}].selection`) || undefined}
                    onChange={(e) => setSelections((s) => s.map((v, j) => (j === i ? e.target.value.toUpperCase() : v)))}
                  />
                </td>
                <td data-bms={`USRID${n}`} className="mono">{row?.userId ?? ''}</td>
                <td data-bms={`FNAME${n}`}>{row?.firstName ?? ''}</td>
                <td data-bms={`LNAME${n}`}>{row?.lastName ?? ''}</td>
                <td data-bms={`UTYPE${n}`}>{row?.userType ?? ''}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </Screen>
  );
}
