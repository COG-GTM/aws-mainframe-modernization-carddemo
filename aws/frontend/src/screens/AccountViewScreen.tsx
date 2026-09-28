import { useState } from 'react';
import { ApiError, errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { Account } from '../api/types';
import { Field, ReadOnly } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { formatSsn, money } from '../lib/format';
import { useBack, useNavState } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { useMountEffect } from '../lib/useMountEffect';
import { blank, isDigits } from '../validation/common';

const INFO_PROMPT = 'Enter or update id of account to display';
const INFO_SHOWN = 'Displaying details of given Account';

export function AccountViewScreen() {
  const back = useBack();
  const focus = useFieldFocus();
  const navState = useNavState() as { acctId?: string };
  const [acctId, setAcctId] = useState(navState.acctId ?? '');
  const [account, setAccount] = useState<Account | null>(null);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [busy, setBusy] = useState(false);

  const search = async (id: string) => {
    setAccount(null);
    if (blank(id)) {
      focus('acct-id');
      return setMessage({ text: 'Account number not provided', kind: 'error' });
    }
    if (!isDigits(id) || id.trim().length > 11 || Number(id) === 0) {
      focus('acct-id');
      return setMessage({ text: 'Account number must be a non zero 11 digit number', kind: 'error' });
    }
    setBusy(true);
    try {
      setAccount(await api.getAccount(id.trim().padStart(11, '0')));
      setMessage(null);
    } catch (err) {
      setMessage({ text: err instanceof ApiError ? err.message : errorMessage(err), kind: 'error' });
      focus('acct-id');
    } finally {
      setBusy(false);
    }
  };

  useMountEffect(() => {
    if (navState.acctId) void search(navState.acctId);
  });

  const c = account?.customer;
  return (
    <Screen
      tranId="CAVW"
      program="COACTVWC"
      title="View Account"
      message={message}
      info={account ? INFO_SHOWN : INFO_PROMPT}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Search', action: () => search(acctId) },
        { key: 'F3', label: 'Exit', action: back },
      ]}
    >
      <div className="form-grid">
        <Field
          id="acct-id"
          label="Account Number :"
          value={acctId}
          onChange={(v) => setAcctId(v.replace(/\D/g, ''))}
          maxLength={11}
          inputMode="numeric"
          autoFocus
          invalid={message?.kind === 'error'}
        />
        <ReadOnly label="Active Y/N:" value={account?.activeStatus ?? ''} width={1} />
      </div>
      <div className="form-grid cols-2">
        <ReadOnly label="Opened:" value={account?.openDate ?? ''} width={10} />
        <ReadOnly label="Credit Limit:" value={money(account?.creditLimit)} width={15} />
        <ReadOnly label="Expiry:" value={account?.expirationDate ?? ''} width={10} />
        <ReadOnly label="Cash credit Limit:" value={money(account?.cashCreditLimit)} width={15} />
        <ReadOnly label="Reissue:" value={account?.reissueDate ?? ''} width={10} />
        <ReadOnly label="Current Balance:" value={money(account?.currBal)} width={15} />
        <ReadOnly label="Account Group:" value={account?.groupId ?? ''} width={10} />
        <ReadOnly label="Current Cycle Credit:" value={money(account?.currCycCredit)} width={15} />
        <span />
        <ReadOnly label="Current Cycle Debit:" value={money(account?.currCycDebit)} width={15} />
      </div>
      <h2 className="section">Customer Details</h2>
      <div className="form-grid cols-3">
        <ReadOnly label="Customer id:" value={c ? String(c.custId).padStart(9, '0') : ''} width={9} />
        <ReadOnly label="SSN:" value={c ? formatSsn(c.ssn) : ''} width={12} />
        <span />
        <ReadOnly label="Date of birth:" value={c?.dob ?? ''} width={10} />
        <ReadOnly label="FICO Score:" value={c?.ficoCreditScore ?? ''} width={3} />
        <span />
        <ReadOnly label="First Name" value={c?.firstName ?? ''} width={25} />
        <ReadOnly label="Middle Name:" value={c?.middleName ?? ''} width={25} />
        <ReadOnly label="Last Name:" value={c?.lastName ?? ''} width={25} />
      </div>
      <div className="form-grid cols-2">
        <ReadOnly label="Address:" value={c?.addrLine1 ?? ''} width={50} />
        <ReadOnly label="State" value={c?.addrStateCd ?? ''} width={2} />
        <ReadOnly label="" value={c?.addrLine2 ?? ''} width={50} />
        <ReadOnly label="Zip" value={c?.addrZip ?? ''} width={5} />
        <ReadOnly label="City" value={c?.addrLine3 ?? ''} width={50} />
        <ReadOnly label="Country" value={c?.addrCountryCd ?? ''} width={3} />
        <ReadOnly label="Phone 1:" value={c?.phoneNum1 ?? ''} width={13} />
        <ReadOnly label="Government Issued Id Ref" value={c?.govtIssuedId ?? ''} width={20} />
        <ReadOnly label="Phone 2:" value={c?.phoneNum2 ?? ''} width={13} />
        <ReadOnly label="EFT Account Id:" value={c?.eftAccountId ?? ''} width={10} />
        <span />
        <ReadOnly label="Primary Card Holder Y/N:" value={c?.priCardHolderInd ?? ''} width={1} />
      </div>
    </Screen>
  );
}
