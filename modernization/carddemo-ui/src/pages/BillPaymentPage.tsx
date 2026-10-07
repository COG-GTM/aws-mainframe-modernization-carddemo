import { useState } from 'react';
import { api } from '../api/endpoints';
import type { BillPaymentResponse } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COBIL00C');

/** COBIL00 / COBIL0A — COBIL00C (CB00). ENTER with confirm blank shows the balance, Y pays it in full. */
export function BillPaymentPage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const fromProgram = handed?.context?.fromProgram ?? null;
  const [accountId, setAccountId] = useState(handed?.context?.acctId ? String(handed.context.acctId).padStart(11, '0') : '');
  const [confirm, setConfirm] = useState('');
  const [result, setResult] = useState<BillPaymentResponse | null>(null);
  const [busy, setBusy] = useState(false);

  const enter = async () => {
    setBusy(true);
    try {
      const sameAccount = result && result.accountId === accountId.trim();
      const answer = await api.billPayment(accountId.trim() || ' ', confirm, sameAccount ? result.version : null, fromProgram);
      setResult(answer);
      setConfirm('');
      msg.say(answer.message, answer.state === 'PAID' ? 'success' : 'info');
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  return (
    <Screen
      page={PAGE}
      header={result?.header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => void enter() },
        { key: 'F3', label: 'Back', action: () => back(PAGE, result?.exit) },
        {
          key: 'F4',
          label: 'Clear',
          action: () => {
            setAccountId('');
            setConfirm('');
            setResult(null);
            msg.clear();
          },
        },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="accountId" bms="ACTIDIN" label="Enter Acct ID" value={accountId} onChange={setAccountId} maxLength={11} autoFocus invalid={msg.isInvalid('accountId')} />
        <Field id="currentBalance" bms="CURBAL" label="Your current balance is" value={result?.currentBalance ?? ''} maxLength={14} readOnly />
        <Field id="confirm" bms="CONFIRM" label="Do you want to pay your balance now. Please confirm" value={confirm} onChange={setConfirm} maxLength={1} upper hint="(Y/N)" invalid={msg.isInvalid('confirm')} />
      </div>
    </Screen>
  );
}
