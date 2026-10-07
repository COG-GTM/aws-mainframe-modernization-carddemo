import { useState } from 'react';
import { api } from '../api/endpoints';
import type { ScreenHeader, TransactionAddRequest } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';
import type { NavigationContext } from '../api/types';

const PAGE = page('COTRN02C');

const EMPTY: TransactionAddRequest = {
  accountId: '',
  cardNumber: '',
  typeCode: '',
  categoryCode: '',
  source: '',
  description: '',
  amount: '',
  origDate: '',
  procDate: '',
  merchantId: '',
  merchantName: '',
  merchantCity: '',
  merchantZip: '',
  confirm: '',
};

/** COTRN02 / COTRN2A — COTRN02C (CT02). ENTER = validate (confirm blank/N) or add (Y); F5 = copyLast. */
export function TransactionAddPage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const fromProgram = handed?.context?.fromProgram ?? null;
  const [form, setForm] = useState<TransactionAddRequest>(() => ({
    ...EMPTY,
    accountId: handed?.context?.acctId ? String(handed.context.acctId).padStart(11, '0') : '',
  }));
  const [header, setHeader] = useState<ScreenHeader | null>(null);
  const [exit, setExit] = useState<NavigationContext | null>(null);
  const [busy, setBusy] = useState(false);

  const submit = async (copyLast: boolean) => {
    setBusy(true);
    try {
      const result = await api.addTransaction({ ...form, copyLast }, fromProgram);
      setHeader(result.header);
      setExit(result.exit);
      if (result.state === 'ADDED') {
        setForm(EMPTY);
        msg.say(result.message, 'success');
      } else {
        if (result.form) setForm({ ...EMPTY, ...result.form, copyLast: undefined });
        msg.say(result.message);
      }
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  const f = (id: keyof TransactionAddRequest, bms: string, label: string, len: number, opts: { numeric?: boolean; upper?: boolean; hint?: string } = {}) => (
    <Field
      id={id}
      bms={bms}
      label={label}
      value={String(form[id] ?? '')}
      onChange={(v) => setForm((cur) => ({ ...cur, [id]: v }))}
      maxLength={len}
      invalid={msg.isInvalid(id)}
      {...opts}
    />
  );

  return (
    <Screen
      page={PAGE}
      header={header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => void submit(false) },
        { key: 'F3', label: 'Back', action: () => back(PAGE, exit) },
        {
          key: 'F4',
          label: 'Clear',
          action: () => {
            setForm(EMPTY);
            msg.clear();
          },
        },
        { key: 'F5', label: 'Copy Last Tran.', action: () => void submit(true) },
      ]}
    >
      <div className="form-grid narrow">
        {f('accountId', 'ACTIDIN', 'Enter Acct #', 11)}
        {f('cardNumber', 'CARDNIN', '(or) Card #', 16)}
      </div>
      <div className="form-grid">
        {f('typeCode', 'TTYPCD', 'Type CD', 2)}
        {f('categoryCode', 'TCATCD', 'Category CD', 4)}
        {f('source', 'TRNSRC', 'Source', 10)}
        {f('description', 'TDESC', 'Description', 60)}
        {f('amount', 'TRNAMT', 'Amount', 12, { hint: '(-99999999.99)' })}
        {f('origDate', 'TORIGDT', 'Orig Date', 10, { hint: '(YYYY-MM-DD)' })}
        {f('procDate', 'TPROCDT', 'Proc Date', 10, { hint: '(YYYY-MM-DD)' })}
        {f('merchantId', 'MID', 'Merchant ID', 9)}
        {f('merchantName', 'MNAME', 'Merchant Name', 30)}
        {f('merchantCity', 'MCITY', 'Merchant City', 25)}
        {f('merchantZip', 'MZIP', 'Merchant Zip', 10)}
      </div>
      <div className="form-grid narrow">
        {f('confirm', 'CONFIRM', 'You are about to add this transaction. Please confirm', 1, { upper: true, hint: '(Y/N)' })}
      </div>
    </Screen>
  );
}
