import { useState } from 'react';
import { api } from '../api/endpoints';
import type { TransactionListScreen } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COTRN00C');
const ROWS = 10;

/** COTRN00 / COTRN0A — COTRN00C (CT00). S on a row = view the transaction. */
export function TransactionListPage() {
  const { back, go } = useProgramNav();
  const msg = useMessages();
  const [startTranId, setStartTranId] = useState('');
  const [list, setList] = useState<TransactionListScreen | null>(null);
  const [selections, setSelections] = useState<string[]>([]);
  const [busy, setBusy] = useState(false);

  const load = async (q: { startTranId?: string; after?: string | null; before?: string | null; page?: number } = { startTranId }) => {
    setBusy(true);
    try {
      const result = await api.transactions(q);
      setList(result);
      setSelections(result.rows.map(() => ''));
      msg.say(result.message);
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    void load({});
  });

  const rows = list?.rows ?? [];

  const enter = async () => {
    if (selections.some((s) => s.trim())) {
      setBusy(true);
      try {
        const result = await api.selectTransaction(rows.map((r, i) => ({ tranId: r.tranId, selection: selections[i] ?? '' })));
        go(result.navigation, { tranId: result.tranId });
      } catch (err) {
        msg.fail(err);
      } finally {
        setBusy(false);
      }
      return;
    }
    await load({ startTranId: startTranId.trim() });
  };

  const forward = () =>
    load({ after: list?.nextPage ?? rows[rows.length - 1]?.tranId ?? startTranId, page: list?.pageNumber });
  const backward = () => load({ before: list?.previousPage ?? rows[0]?.tranId ?? startTranId, page: list?.pageNumber });

  return (
    <Screen
      page={PAGE}
      header={list?.header}
      message={msg.message}
      busy={busy}
      info="Type 'S' to View Transaction details from the list"
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
        <Field id="startTranId" bms="TRNIDIN" label="Search Tran ID" value={startTranId} onChange={setStartTranId} maxLength={16} autoFocus invalid={msg.isInvalid('startTranId')} />
      </div>
      <table className="grid">
        <thead>
          <tr>
            <th>Sel</th>
            <th>Transaction ID</th>
            <th>Date</th>
            <th>Description</th>
            <th className="num">Amount</th>
          </tr>
        </thead>
        <tbody>
          {Array.from({ length: ROWS }, (_, i) => {
            const row = rows[i];
            const n = String(i + 1).padStart(2, '0');
            return (
              <tr key={i} data-testid={`tran-row-${i + 1}`}>
                <td>
                  {row && (
                    <input
                      id={`rows[${i}].selection`}
                      data-bms={`SEL00${n}`}
                      aria-label={`Select row ${i + 1}`}
                      className="sel"
                      maxLength={1}
                      value={selections[i] ?? ''}
                      aria-invalid={msg.isInvalid(`rows[${i}].selection`) || undefined}
                      onChange={(e) => setSelections((s) => s.map((v, j) => (j === i ? e.target.value.toUpperCase() : v)))}
                    />
                  )}
                </td>
                <td data-bms={`TRNID${n}`} className="mono">{row?.tranId ?? ''}</td>
                <td data-bms={`TDATE${n}`}>{row?.date ?? ''}</td>
                <td data-bms={`TDESC${n}`}>{row?.description ?? ''}</td>
                <td data-bms={`TAMT0${n}`} className="num mono">{row?.amount ?? ''}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </Screen>
  );
}
