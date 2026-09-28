import { useState } from 'react';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { BillBalance } from '../api/types';
import { Field, ReadOnly } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { money } from '../lib/format';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { blank, isDigits } from '../validation/common';

export function BillPaymentScreen() {
  const back = useBack();
  const focus = useFieldFocus();
  const [acctId, setAcctId] = useState('');
  const [confirm, setConfirm] = useState('');
  const [balance, setBalance] = useState<BillBalance | null>(null);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<'acctId' | 'confirm' | null>(null);
  const [busy, setBusy] = useState(false);

  const error = (field: 'acctId' | 'confirm', text: string, kind: ScreenMessage['kind'] = 'error') => {
    setInvalid(kind === 'error' ? field : null);
    setMessage({ text, kind });
    focus(field === 'acctId' ? 'bp-acct' : 'bp-confirm');
  };

  const clear = () => {
    setAcctId('');
    setConfirm('');
    setBalance(null);
    setInvalid(null);
    setMessage(null);
    focus('bp-acct');
  };

  /** COBIL00C PROCESS-ENTER-KEY. */
  const enter = async () => {
    if (blank(acctId)) return error('acctId', 'Acct ID can NOT be empty...');
    const c = confirm.trim().toUpperCase();
    if (c === 'N') return clear();
    if (c !== '' && c !== 'Y') return error('confirm', 'Invalid value. Valid values are (Y/N)...');
    if (!isDigits(acctId)) return error('acctId', 'Account ID NOT found...');
    setBusy(true);
    try {
      const current = await api.getBillBalance(acctId.trim());
      setBalance(current);
      if (Number(current.currBal) <= 0) return error('acctId', 'You have nothing to pay...');
      if (c !== 'Y') return error('confirm', 'Confirm to make a bill payment...', 'info');
      const paid = await api.payBill(current.acctId);
      setBalance({ ...current, currBal: '0.00' });
      setConfirm('');
      setInvalid(null);
      setMessage({ text: paid.message, kind: 'success' });
    } catch (err) {
      error('acctId', errorMessage(err));
    } finally {
      setBusy(false);
    }
  };

  return (
    <Screen
      tranId="CB00"
      program="COBIL00C"
      title="Bill Payment"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: enter },
        { key: 'F3', label: 'Back', action: back },
        { key: 'F4', label: 'Clear', action: clear },
      ]}
    >
      <div className="form-grid">
        <Field
          id="bp-acct"
          label="Enter Acct ID:"
          value={acctId}
          onChange={(v) => setAcctId(v.replace(/\D/g, ''))}
          maxLength={11}
          inputMode="numeric"
          autoFocus
          invalid={invalid === 'acctId'}
        />
      </div>
      <hr />
      <div className="form-grid">
        <ReadOnly label="Your current balance is:" value={balance ? money(balance.currBal) : ''} width={14} />
      </div>
      <div className="form-grid confirm-row">
        <Field
          id="bp-confirm"
          label="Do you want to pay your balance now. Please confirm:"
          value={confirm}
          onChange={setConfirm}
          maxLength={1}
          hint="(Y/N)"
          upper
          invalid={invalid === 'confirm'}
        />
      </div>
    </Screen>
  );
}
