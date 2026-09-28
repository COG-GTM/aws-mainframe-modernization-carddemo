import { useState } from 'react';
import { ApiError, errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { Account } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import {

  fromForm,
  toForm,
  validateAccountForm,
  type AccountField,
  type AccountForm,
} from '../validation/account';
import { blank, isDigits } from '../validation/common';

type Phase = 'search' | 'edit' | 'validated';

const INFO: Record<Phase, string> = {
  search: 'Enter or update id of account to update',
  edit: 'Update account details presented above.',
  validated: 'Changes validated.Press F5 to save',
};

const fieldId = (f: AccountField) => `acup-${f}`;

export function AccountUpdateScreen() {
  const back = useBack();
  const focus = useFieldFocus();
  const [acctId, setAcctId] = useState('');
  const [account, setAccount] = useState<Account | null>(null);
  const [original, setOriginal] = useState<AccountForm | null>(null);
  const [form, setForm] = useState<AccountForm | null>(null);
  const [phase, setPhase] = useState<Phase>('search');
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<AccountField | 'acctId' | null>(null);
  const [busy, setBusy] = useState(false);

  const error = (field: AccountField | 'acctId', text: string) => {
    setInvalid(field);
    setMessage({ text, kind: 'error' });
    focus(field === 'acctId' ? 'acup-acct-id' : fieldId(field));
  };

  const load = async () => {
    if (blank(acctId)) return error('acctId', 'Account number not provided');
    if (!isDigits(acctId) || acctId.length > 11 || Number(acctId) === 0) {
      return error('acctId', 'Account number must be a non zero 11 digit number');
    }
    setBusy(true);
    try {
      const fetched = await api.getAccount(acctId.padStart(11, '0'));
      const image = toForm(fetched);
      setAccount(fetched);
      setOriginal(image);
      setForm(image);
      setAcctId(image.acctId);
      setPhase('edit');
      setInvalid(null);
      setMessage(null);
      focus(fieldId('activeStatus'));
    } catch (err) {
      setAccount(null);
      setForm(null);
      setPhase('search');
      error('acctId', errorMessage(err));
    } finally {
      setBusy(false);
    }
  };

  const validate = (): boolean => {
    if (!form || !original) return false;
    const problem = validateAccountForm(form, original);
    if (problem) {
      error(problem.field, problem.message);
      setPhase('edit');
      return false;
    }
    setInvalid(null);
    setMessage(null);
    setPhase('validated');
    return true;
  };

  const enter = () => {
    if (!account || (form && original && acctId !== original.acctId)) return void load();
    validate();
  };

  const save = async () => {
    if (!account || !form) return;
    if (phase !== 'validated' && !validate()) return;
    setBusy(true);
    try {
      const saved = await api.updateAccount(form.acctId, fromForm(form, account));
      const image = toForm(saved);
      setAccount(saved);
      setOriginal(image);
      setForm(image);
      setPhase('edit');
      setInvalid(null);
      setMessage({ text: 'Changes committed to database', kind: 'success' });
    } catch (err) {
      const text = errorMessage(err);
      if (err instanceof ApiError && err.status === 409) {
        setMessage({ text, kind: 'error' });
        setPhase('edit');
        try {
          const fresh = await api.getAccount(form.acctId);
          setAccount(fresh);
          setOriginal(toForm(fresh));
          setForm(toForm(fresh));
        } catch {
          // keep the user's image when the refresh fails
        }
      } else {
        setMessage({ text: text || 'Changes unsuccessful. Please try again', kind: 'error' });
      }
    } finally {
      setBusy(false);
    }
  };

  const cancel = () => {
    if (original) setForm(original);
    setPhase(account ? 'edit' : 'search');
    setInvalid(null);
    setMessage(null);
  };

  const set = (field: AccountField) => (value: string) => {
    setForm((f) => (f ? { ...f, [field]: value } : f));
    if (phase === 'validated') setPhase('edit');
  };

  const input = (field: AccountField, label: string, maxLength: number, extra: { upper?: boolean; numeric?: boolean } = {}) => (
    <Field
      id={fieldId(field)}
      label={label}
      value={form?.[field] ?? ''}
      onChange={set(field)}
      maxLength={maxLength}
      readOnly={!form}
      invalid={invalid === field}
      upper={extra.upper}
      inputMode={extra.numeric ? 'numeric' : undefined}
    />
  );

  const dateGroup = (label: string, y: AccountField, m: AccountField, d: AccountField) => (
    <fieldset className="field date-group">
      <legend>{label}</legend>
      <input
        id={fieldId(y)}
        aria-label={`${label} year`}
        value={form?.[y] ?? ''}
        onChange={(e) => set(y)(e.target.value)}
        maxLength={4}
        readOnly={!form}
        aria-invalid={invalid === y || undefined}
        className="w4"
      />
      -
      <input
        id={fieldId(m)}
        aria-label={`${label} month`}
        value={form?.[m] ?? ''}
        onChange={(e) => set(m)(e.target.value)}
        maxLength={2}
        readOnly={!form}
        aria-invalid={invalid === m || undefined}
        className="w2"
      />
      -
      <input
        id={fieldId(d)}
        aria-label={`${label} day`}
        value={form?.[d] ?? ''}
        onChange={(e) => set(d)(e.target.value)}
        maxLength={2}
        readOnly={!form}
        aria-invalid={invalid === d || undefined}
        className="w2"
      />
    </fieldset>
  );

  const triple = (label: string, parts: [AccountField, number, string][], sep: [string, string, string, string]) => (
    <fieldset className="field date-group">
      <legend>{label}</legend>
      {sep[0]}
      {parts.map(([f, len, name], i) => (
        <span key={f}>
          <input
            id={fieldId(f)}
            aria-label={`${label} ${name}`}
            value={form?.[f] ?? ''}
            onChange={(e) => set(f)(e.target.value)}
            maxLength={len}
            readOnly={!form}
            aria-invalid={invalid === f || undefined}
            className={`w${len}`}
          />
          {sep[i + 1]}
        </span>
      ))}
    </fieldset>
  );

  return (
    <Screen
      tranId="CAUP"
      program="COACTUPC"
      title="Update Account"
      message={message}
      info={INFO[phase]}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Process', action: enter },
        { key: 'F3', label: 'Exit', action: back },
        { key: 'F5', label: 'Save', action: save, disabled: !account },
        { key: 'F12', label: 'Cancel', action: cancel, disabled: !account },
      ]}
    >
      <div className="form-grid">
        <Field
          id="acup-acct-id"
          label="Account Number :"
          value={acctId}
          onChange={(v) => setAcctId(v.replace(/\D/g, ''))}
          maxLength={11}
          inputMode="numeric"
          autoFocus
          invalid={invalid === 'acctId'}
        />
        {input('activeStatus', 'Active Y/N:', 1, { upper: true })}
      </div>
      <div className="form-grid cols-2">
        {dateGroup('Opened :', 'openYear', 'openMon', 'openDay')}
        {input('creditLimit', 'Credit Limit :', 15)}
        {dateGroup('Expiry Date :', 'expYear', 'expMon', 'expDay')}
        {input('cashCreditLimit', 'Cash credit Limit :', 15)}
        {dateGroup('Reissue Date :', 'risYear', 'risMon', 'risDay')}
        {input('currBal', 'Current Balance :', 15)}
        {input('groupId', 'Account Group :', 10)}
        {input('currCycCredit', 'Current Cycle Credit:', 15)}
        <span />
        {input('currCycDebit', 'Current Cycle Debit :', 15)}
      </div>
      <h2 className="section">Customer Details</h2>
      <div className="form-grid cols-3">
        <Field id="acup-custId" label="Customer id :" value={form?.custId ?? ''} readOnly maxLength={9} />
        {triple('SSN :', [['ssn1', 3, 'part 1'], ['ssn2', 2, 'part 2'], ['ssn3', 4, 'part 3']], ['', '-', '-', ''])}
        <span />
        {dateGroup('Date of birth :', 'dobYear', 'dobMon', 'dobDay')}
        {input('fico', 'FICO Score:', 3, { numeric: true })}
        <span />
        {input('firstName', 'First Name', 25)}
        {input('middleName', 'Middle Name:', 25)}
        {input('lastName', 'Last Name:', 25)}
      </div>
      <div className="form-grid cols-2">
        {input('addrLine1', 'Address:', 50)}
        {input('addrStateCd', 'State', 2, { upper: true })}
        {input('addrLine2', 'Address line 2', 50)}
        {input('addrZip', 'Zip', 5, { numeric: true })}
        {input('addrLine3', 'City', 50)}
        {input('addrCountryCd', 'Country', 3, { upper: true })}
        {triple('Phone 1:', [['ph1a', 3, 'area code'], ['ph1b', 3, 'prefix'], ['ph1c', 4, 'line number']], ['(', ')', '-', ''])}
        {input('govtIssuedId', 'Government Issued Id Ref :', 20)}
        {triple('Phone 2:', [['ph2a', 3, 'area code'], ['ph2b', 3, 'prefix'], ['ph2c', 4, 'line number']], ['(', ')', '-', ''])}
        {input('eftAccountId', 'EFT Account Id:', 10, { numeric: true })}
        <span />
        {input('priCardHolderInd', 'Primary Card Holder Y/N:', 1, { upper: true })}
      </div>
    </Screen>
  );
}
