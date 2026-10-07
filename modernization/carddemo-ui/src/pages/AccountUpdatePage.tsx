import { useState } from 'react';
import { ApiError } from '../api/client';
import { api } from '../api/endpoints';
import type { AccountUpdateRequest, AccountViewScreen, UpdateState } from '../api/types';
import { Field, FieldGroup, Part } from '../components/Field';
import { Screen, type PfKey } from '../components/Screen';
import { text } from '../lib/format';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COACTUPC');

type Form = AccountUpdateRequest;

function setPath(form: Form, path: string, value: string): Form {
  const [head, tail] = path.split('.');
  if (!tail) return { ...form, [head]: value };
  const nested = (form as unknown as Record<string, Record<string, string>>)[head];
  return { ...form, [head]: { ...nested, [tail]: value } };
}

function getPath(form: Form | null, path: string): string {
  if (!form) return '';
  const [head, tail] = path.split('.');
  const top = (form as unknown as Record<string, unknown>)[head];
  if (!tail) return text(top);
  return text((top as Record<string, unknown> | null)?.[tail]);
}

/** COACTUP / CACTUPA — COACTUPC (CAUP). ENTER = PUT confirm=false, F5 = PUT confirm=true, F12 = re-fetch. */
export function AccountUpdatePage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program)?.context?.acctId;
  const [acctId, setAcctId] = useState(handed ? String(handed).padStart(11, '0') : '');
  const [account, setAccount] = useState<AccountViewScreen | null>(null);
  const [form, setForm] = useState<Form | null>(null);
  const [state, setState] = useState<UpdateState | null>(null);
  const [info, setInfo] = useState<string>('Enter or update id of account to update');
  const [busy, setBusy] = useState(false);

  const show = (result: AccountViewScreen) => {
    setAccount(result);
    setForm(result.updateForm);
    setAcctId(result.acctId);
    setState('SHOW');
    setInfo(result.infoMessage || 'Update account details presented above.');
  };

  const fetch = async (id: string = acctId) => {
    setBusy(true);
    try {
      const result = await api.account(id.trim() || ' ');
      show(result);
      msg.say(result.message);
    } catch (err) {
      setAccount(null);
      setForm(null);
      setState(null);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed) void fetch(String(handed));
  });

  const submit = async (confirm: boolean) => {
    if (!account || !form) return fetch();
    setBusy(true);
    try {
      const result = await api.updateAccount(account.acctId, { ...form, confirm });
      setState(result.state);
      setInfo(result.infoMessage);
      if (result.state === 'COMMITTED' && result.account) {
        setAccount(result.account);
        setForm(result.account.updateForm);
      }
      msg.say(result.message, result.state === 'COMMITTED' ? 'success' : 'info');
    } catch (err) {
      if (err instanceof ApiError && err.status === 409) {
        try {
          show(await api.account(account.acctId));
        } catch {
          // keep the conflict message
        }
      } else if (state === 'VALIDATED') {
        setState('SHOW');
      }
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  const edit = (path: string) => (value: string) => {
    if (!form) return;
    setForm(setPath(form, path, value));
    if (state !== 'SHOW') setState('SHOW');
  };

  const fetched = account !== null;
  const editable = fetched && (state === 'SHOW' || state === 'VALIDATED');
  const f = (id: string, bms: string, label: string, len: number, opts: { numeric?: boolean; upper?: boolean } = {}) => (
    <Field id={id} bms={bms} label={label} value={getPath(form, id)} onChange={edit(id)} maxLength={len} readOnly={!editable} invalid={msg.isInvalid(id)} {...opts} />
  );
  const p = (id: string, bms: string, label: string, len: number) => (
    <Part id={id} bms={bms} label={label} value={getPath(form, id)} onChange={edit(id)} maxLength={len} readOnly={!editable} invalid={msg.isInvalid(id)} />
  );
  const date = (base: string, bms: string, label: string) => (
    <FieldGroup label={label} htmlFor={`${base}.year`}>
      {p(`${base}.year`, `${bms}YEAR`, `${label} year`, 4)}
      <span>-</span>
      {p(`${base}.month`, `${bms}MON`, `${label} month`, 2)}
      <span>-</span>
      {p(`${base}.day`, `${bms}DAY`, `${label} day`, 2)}
    </FieldGroup>
  );
  const phone = (base: string, bms: string, label: string) => (
    <FieldGroup label={label} htmlFor={`${base}.areaCode`}>
      {p(`${base}.areaCode`, `${bms}A`, `${label} area code`, 3)}
      {p(`${base}.prefix`, `${bms}B`, `${label} prefix`, 3)}
      {p(`${base}.lineNumber`, `${bms}C`, `${label} line number`, 4)}
    </FieldGroup>
  );

  const pfKeys: PfKey[] = [
    { key: 'ENTER', label: 'Process', action: () => void (fetched ? submit(false) : fetch()) },
    { key: 'F3', label: 'Exit', action: () => back(PAGE, account?.exit) },
  ];
  if (state === 'VALIDATED') pfKeys.push({ key: 'F5', label: 'Save', action: () => void submit(true) });
  if (fetched) pfKeys.push({ key: 'F12', label: 'Cancel', action: () => void fetch(account.acctId) });

  return (
    <Screen page={PAGE} header={account?.header} message={msg.message} info={info} busy={busy} pfKeys={pfKeys}>
      <div className="form-grid">
        <Field id="accountId" bms="ACCTSID" label="Account Number" value={acctId} onChange={setAcctId} maxLength={11} readOnly={fetched} autoFocus invalid={msg.isInvalid('accountId')} />
        {f('activeStatus', 'ACSTTUS', 'Active Y/N', 1, { upper: true })}
        {date('openDate', 'OPN', 'Opened')}
        {f('creditLimit', 'ACRDLIM', 'Credit Limit', 15)}
        {date('expiryDate', 'EXP', 'Expiry')}
        {f('cashCreditLimit', 'ACSHLIM', 'Cash credit Limit', 15)}
        {date('reissueDate', 'RIS', 'Reissue')}
        {f('currentBalance', 'ACURBAL', 'Current Balance', 15)}
        {f('currentCycleCredit', 'ACRCYCR', 'Current Cycle Credit', 15)}
        {f('groupId', 'AADDGRP', 'Account Group', 10)}
        {f('currentCycleDebit', 'ACRCYDB', 'Current Cycle Debit', 15)}
      </div>
      <h2 className="section">Customer Details</h2>
      <div className="form-grid">
        <Field id="custId" bms="ACSTNUM" label="Customer id" value={text(account?.custId)} maxLength={9} readOnly />
        <FieldGroup label="SSN" htmlFor="ssn.part1">
          {p('ssn.part1', 'ACTSSN1', 'SSN part 1', 3)}
          <span>-</span>
          {p('ssn.part2', 'ACTSSN2', 'SSN part 2', 2)}
          <span>-</span>
          {p('ssn.part3', 'ACTSSN3', 'SSN part 3', 4)}
        </FieldGroup>
        {date('dateOfBirth', 'DOB', 'Date of birth')}
        {f('ficoScore', 'ACSTFCO', 'FICO Score', 3)}
        {f('firstName', 'ACSFNAM', 'First Name', 25)}
        {f('middleName', 'ACSMNAM', 'Middle Name', 25)}
        {f('lastName', 'ACSLNAM', 'Last Name', 25)}
        {f('addressLine1', 'ACSADL1', 'Address', 50)}
        {f('addressLine2', 'ACSADL2', 'Address line 2', 50)}
        {f('state', 'ACSSTTE', 'State', 2, { upper: true })}
        {f('zip', 'ACSZIPC', 'Zip', 5)}
        {f('city', 'ACSCITY', 'City', 50)}
        <Field id="country" bms="ACSCTRY" label="Country" value={text(account?.country)} maxLength={3} readOnly />
        {phone('phone1', 'ACSPH1', 'Phone 1')}
        {f('governmentId', 'ACSGOVT', 'Government Issued Id Ref', 20)}
        {phone('phone2', 'ACSPH2', 'Phone 2')}
        {f('eftAccountId', 'ACSEFTC', 'EFT Account Id', 10)}
        {f('primaryCardHolder', 'ACSPFLG', 'Primary Card Holder Y/N', 1, { upper: true })}
      </div>
    </Screen>
  );
}
