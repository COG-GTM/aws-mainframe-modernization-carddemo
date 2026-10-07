import { useState } from 'react';
import { api } from '../api/endpoints';
import type { CardListScreen } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';
import { useSession } from '../session/session';

const PAGE = page('COCRDLIC');
const ROWS = 7;

/** COCRDLI / CCRDLIA — COCRDLIC (CCLI). Masked PAN + opaque cardRef per row (ADR-0020); S = detail, U = update. */
export function CardListPage() {
  const { back, go, incoming } = useProgramNav();
  const { session } = useSession();
  const msg = useMessages();
  const handed = incoming(PAGE.program)?.context?.acctId;
  const [accountId, setAccountId] = useState(handed ? String(handed).padStart(11, '0') : '');
  const [cardNumber, setCardNumber] = useState('');
  const [list, setList] = useState<CardListScreen | null>(null);
  const [selections, setSelections] = useState<string[]>([]);
  const [pageNo, setPageNo] = useState(1);
  const [busy, setBusy] = useState(false);

  const load = async (cursor: { after?: string | null; before?: string | null } = {}, nextPage = 1) => {
    setBusy(true);
    try {
      const result = await api.cards({ accountId: accountId.trim(), cardNumber: cardNumber.trim(), ...cursor });
      if ((cursor.after || cursor.before) && result.rows.length === 0 && list?.rows.length) {
        // Past the first/last page: keep the rows on screen and show the program's message.
        setList({ ...list, hasNextPage: cursor.after ? false : list.hasNextPage, hasPreviousPage: cursor.before ? false : list.hasPreviousPage });
      } else {
        setList(result);
        setSelections(result.rows.map(() => ''));
        setPageNo(nextPage);
      }
      msg.say(result.message);
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (session?.role === 'ADMIN' || accountId) void load();
  });

  const enter = async () => {
    const rows = list?.rows ?? [];
    if (selections.some((s) => s.trim())) {
      setBusy(true);
      try {
        const result = await api.selectCard(
          accountId.trim() || undefined,
          rows.map((r, i) => ({ cardRef: r.cardRef, action: selections[i] ?? '' })),
        );
        go(result.navigation, { cardRef: result.cardRef });
      } catch (err) {
        msg.fail(err);
      } finally {
        setBusy(false);
      }
      return;
    }
    await load();
  };

  const rows = list?.rows ?? [];
  const forward = () => {
    if (!list) return load();
    return load({ after: list.nextPage ?? rows[rows.length - 1]?.cardRef }, list.hasNextPage ? pageNo + 1 : pageNo);
  };
  const backward = () => {
    if (!list) return load();
    return load({ before: list.previousPage ?? rows[0]?.cardRef }, list.hasPreviousPage ? Math.max(1, pageNo - 1) : pageNo);
  };

  return (
    <Screen
      page={PAGE}
      header={list?.header}
      message={msg.message}
      info={list?.infoMessage}
      busy={busy}
      titleExtra={
        <span className="page-no">
          Page <span data-bms="PAGENO">{list ? pageNo : ''}</span>
        </span>
      }
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => void enter() },
        { key: 'F3', label: 'Exit', action: () => back(PAGE, list?.exit) },
        { key: 'F7', label: 'Backward', action: () => void backward() },
        { key: 'F8', label: 'Forward', action: () => void forward() },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="accountId" bms="ACCTSID" label="Account Number" value={accountId} onChange={setAccountId} maxLength={11} autoFocus invalid={msg.isInvalid('accountId')} />
        <Field id="cardNumber" bms="CARDSID" label="Credit Card Number" value={cardNumber} onChange={setCardNumber} maxLength={16} invalid={msg.isInvalid('cardNumber')} />
      </div>
      <table className="grid">
        <thead>
          <tr>
            <th>Select</th>
            <th>Account Number</th>
            <th>Card Number</th>
            <th>Active</th>
          </tr>
        </thead>
        <tbody>
          {Array.from({ length: ROWS }, (_, i) => {
            const row = rows[i];
            const n = i + 1;
            return (
              <tr key={i} data-testid={`card-row-${n}`}>
                <td>
                  <input
                    disabled={!row}
                    id={`rows[${i}].action`}
                    data-bms={`CRDSEL${n}`}
                    aria-label={`Select row ${n}`}
                    className="sel"
                    maxLength={1}
                    value={selections[i] ?? ''}
                    aria-invalid={msg.isInvalid(`rows[${i}].action`) || undefined}
                    onChange={(e) => setSelections((s) => s.map((v, j) => (j === i ? e.target.value.toUpperCase() : v)))}
                  />
                </td>
                <td data-bms={`ACCTNO${n}`}>{row?.accountId ?? ''}</td>
                <td data-bms={`CRDNUM${n}`} className="mono">
                  {row?.cardNumber ?? ''}
                </td>
                <td data-bms={`CRDSTS${n}`}>{row?.activeStatus ?? ''}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </Screen>
  );
}
