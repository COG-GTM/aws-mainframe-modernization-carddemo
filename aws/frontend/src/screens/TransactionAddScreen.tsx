import { useState } from 'react';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { blank } from '../validation/common';
import {
  CONFIRM_ADD,
  EMPTY_TRANSACTION_FORM,
  INVALID_YN,
  toNewTransaction,
  validateTransactionAdd,
  type TransactionAddField,
  type TransactionAddForm,
} from '../validation/transaction';

export function TransactionAddScreen() {
  const back = useBack();
  const focus = useFieldFocus();
  const [form, setForm] = useState<TransactionAddForm>(EMPTY_TRANSACTION_FORM);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<TransactionAddField | null>(null);
  const [busy, setBusy] = useState(false);

  const error = (field: TransactionAddField, text: string, kind: ScreenMessage['kind'] = 'error') => {
    setInvalid(kind === 'error' ? field : null);
    setMessage({ text, kind });
    focus(`ta-${field}`);
  };

  const enter = async () => {
    const problem = validateTransactionAdd(form);
    if (problem) return error(problem.field, problem.message);
    const confirm = form.confirm.trim().toUpperCase();
    if (blank(confirm) || confirm === 'N') return error('confirm', CONFIRM_ADD, 'info');
    if (confirm !== 'Y') return error('confirm', INVALID_YN);
    setBusy(true);
    try {
      const created = await api.addTransaction(toNewTransaction(form));
      setForm(EMPTY_TRANSACTION_FORM);
      setInvalid(null);
      setMessage({ text: created.message, kind: 'success' });
      focus('ta-acctId');
    } catch (err) {
      setMessage({ text: errorMessage(err), kind: 'error' });
    } finally {
      setBusy(false);
    }
  };

  /** COTRN02C COPY-LAST-TRAN-DATA: prefill the data fields from the most recent transaction. */
  const copyLast = async () => {
    const key = validateTransactionAdd({ ...form, typeCd: 'x' });
    if (key && (key.field === 'acctId' || key.field === 'cardNum')) return error(key.field, key.message);
    setBusy(true);
    try {
      const last = await api.listTransactions({ direction: 'prev', pageSize: 1 });
      const summary = last.items[last.items.length - 1];
      if (!summary) return setMessage({ text: 'Transaction ID NOT found...', kind: 'error' });
      const t = await api.getTransaction(summary.tranId);
      setForm((f) => ({
        ...f,
        typeCd: t.typeCd,
        catCd: String(t.catCd).padStart(4, '0'),
        source: t.source,
        description: t.description,
        amt: `${Number(t.amt) < 0 ? '-' : '+'}${Math.abs(Number(t.amt)).toFixed(2).padStart(11, '0')}`,
        origDate: t.origTs.slice(0, 10),
        procDate: t.procTs.slice(0, 10),
        merchantId: String(t.merchantId).padStart(9, '0'),
        merchantName: t.merchantName,
        merchantCity: t.merchantCity,
        merchantZip: t.merchantZip,
      }));
      setInvalid(null);
      setMessage(null);
      focus('ta-confirm');
    } catch (err) {
      setMessage({ text: errorMessage(err), kind: 'error' });
    } finally {
      setBusy(false);
    }
  };

  const clear = () => {
    setForm(EMPTY_TRANSACTION_FORM);
    setInvalid(null);
    setMessage(null);
    focus('ta-acctId');
  };

  const set = (field: TransactionAddField) => (value: string) => setForm((f) => ({ ...f, [field]: value }));
  const input = (field: TransactionAddField, label: string, maxLength: number, hint?: string, autoFocus?: boolean) => (
    <Field
      id={`ta-${field}`}
      label={label}
      value={form[field]}
      onChange={set(field)}
      maxLength={maxLength}
      hint={hint}
      invalid={invalid === field}
      autoFocus={autoFocus}
    />
  );

  return (
    <Screen
      tranId="CT02"
      program="COTRN02C"
      title="Add Transaction"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: enter },
        { key: 'F3', label: 'Back', action: back },
        { key: 'F4', label: 'Clear', action: clear },
        { key: 'F5', label: 'Copy Last Tran.', action: copyLast },
      ]}
    >
      <div className="form-grid">
        {input('acctId', 'Enter Acct #:', 11, undefined, true)}
        <span className="or">(or)</span>
        {input('cardNum', 'Card #:', 16)}
      </div>
      <hr />
      <div className="form-grid cols-3">
        {input('typeCd', 'Type CD:', 2)}
        {input('catCd', 'Category CD:', 4)}
        {input('source', 'Source:', 10)}
      </div>
      <div className="form-grid">{input('description', 'Description:', 60)}</div>
      <div className="form-grid cols-3">
        {input('amt', 'Amount:', 12, '(-99999999.99)')}
        {input('origDate', 'Orig Date:', 10, '(YYYY-MM-DD)')}
        {input('procDate', 'Proc Date:', 10, '(YYYY-MM-DD)')}
        {input('merchantId', 'Merchant ID:', 9)}
        {input('merchantName', 'Merchant Name:', 30)}
        <span />
        {input('merchantCity', 'Merchant City:', 25)}
        {input('merchantZip', 'Merchant Zip:', 10)}
      </div>
      <div className="form-grid confirm-row">
        <Field
          id="ta-confirm"
          label="You are about to add this transaction. Please confirm :"
          value={form.confirm}
          onChange={set('confirm')}
          maxLength={1}
          hint="(Y/N)"
          upper
          invalid={invalid === 'confirm'}
        />
      </div>
    </Screen>
  );
}
