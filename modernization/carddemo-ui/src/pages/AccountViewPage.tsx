import { useState } from 'react';
import { api } from '../api/endpoints';
import type { AccountViewScreen } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { money, text } from '../lib/format';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COACTVWC');

/** COACTVW / CACTVWA — COACTVWC (CAVW). */
export function AccountViewPage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program)?.context?.acctId;
  const [acctId, setAcctId] = useState(handed ? String(handed).padStart(11, '0') : '');
  const [view, setView] = useState<AccountViewScreen | null>(null);
  const [busy, setBusy] = useState(false);

  const load = async (id: string = acctId) => {
    setBusy(true);
    try {
      const result = await api.account(id.trim() || ' ');
      setView(result);
      setAcctId(result.acctId);
      msg.say(result.message);
    } catch (err) {
      setView(null);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed) void load(String(handed));
  });

  const v = view;
  const ro = (id: string, bms: string, label: string, value: unknown, len: number) => (
    <Field id={id} bms={bms} label={label} value={text(value)} maxLength={len} readOnly />
  );

  return (
    <Screen
      page={PAGE}
      header={v?.header}
      message={msg.message}
      info={v ? v.infoMessage : 'Enter or update id of account to display'}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => void load() },
        { key: 'F3', label: 'Exit', action: () => back(PAGE, v?.exit) },
      ]}
    >
      <div className="form-grid">
        <Field id="accountId" bms="ACCTSID" label="Account Number" value={acctId} onChange={setAcctId} maxLength={11} autoFocus invalid={msg.isInvalid('accountId')} />
        {ro('activeStatus', 'ACSTTUS', 'Active Y/N', v?.activeStatus, 1)}
        {ro('openDate', 'ADTOPEN', 'Opened', v?.openDate, 10)}
        {ro('creditLimit', 'ACRDLIM', 'Credit Limit', money(v?.creditLimit), 15)}
        {ro('expirationDate', 'AEXPDT', 'Expiry', v?.expirationDate, 10)}
        {ro('cashCreditLimit', 'ACSHLIM', 'Cash credit Limit', money(v?.cashCreditLimit), 15)}
        {ro('reissueDate', 'AREISDT', 'Reissue', v?.reissueDate, 10)}
        {ro('currentBalance', 'ACURBAL', 'Current Balance', money(v?.currentBalance), 15)}
        {ro('currentCycleCredit', 'ACRCYCR', 'Current Cycle Credit', money(v?.currentCycleCredit), 15)}
        {ro('groupId', 'AADDGRP', 'Account Group', v?.groupId, 10)}
        {ro('currentCycleDebit', 'ACRCYDB', 'Current Cycle Debit', money(v?.currentCycleDebit), 15)}
      </div>
      <h2 className="section">Customer Details</h2>
      <div className="form-grid">
        {ro('custId', 'ACSTNUM', 'Customer id', v?.custId, 9)}
        {ro('ssn', 'ACSTSSN', 'SSN', v?.ssn, 12)}
        {ro('dateOfBirth', 'ACSTDOB', 'Date of birth', v?.dateOfBirth, 10)}
        {ro('ficoScore', 'ACSTFCO', 'FICO Score', v?.ficoScore, 3)}
        {ro('firstName', 'ACSFNAM', 'First Name', v?.firstName, 25)}
        {ro('middleName', 'ACSMNAM', 'Middle Name', v?.middleName, 25)}
        {ro('lastName', 'ACSLNAM', 'Last Name', v?.lastName, 25)}
        {ro('addressLine1', 'ACSADL1', 'Address', v?.addressLine1, 50)}
        {ro('addressLine2', 'ACSADL2', 'Address line 2', v?.addressLine2, 50)}
        {ro('state', 'ACSSTTE', 'State', v?.state, 2)}
        {ro('zip', 'ACSZIPC', 'Zip', v?.zip, 5)}
        {ro('city', 'ACSCITY', 'City', v?.city, 50)}
        {ro('country', 'ACSCTRY', 'Country', v?.country, 3)}
        {ro('phone1', 'ACSPHN1', 'Phone 1', v?.phone1, 13)}
        {ro('governmentId', 'ACSGOVT', 'Government Issued Id Ref', v?.governmentId, 20)}
        {ro('phone2', 'ACSPHN2', 'Phone 2', v?.phone2, 13)}
        {ro('eftAccountId', 'ACSEFTC', 'EFT Account Id', v?.eftAccountId, 10)}
        {ro('primaryCardHolder', 'ACSPFLG', 'Primary Card Holder Y/N', v?.primaryCardHolder, 1)}
      </div>
    </Screen>
  );
}
