import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { Transaction } from '../api/types';
import { Field, ReadOnly } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack, useNavState } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { useMountEffect } from '../lib/useMountEffect';
import { blank } from '../validation/common';

export function TransactionDetailScreen() {
  const back = useBack();
  const navigate = useNavigate();
  const focus = useFieldFocus();
  const navState = useNavState() as { tranId?: string; from?: string };
  const [tranId, setTranId] = useState(navState.tranId ?? '');
  const [tran, setTran] = useState<Transaction | null>(null);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [busy, setBusy] = useState(false);

  const fetchTran = async (id: string) => {
    setTran(null);
    if (blank(id)) {
      focus('tv-id');
      return setMessage({ text: 'Tran ID can NOT be empty...', kind: 'error' });
    }
    setBusy(true);
    try {
      setTran(await api.getTransaction(id.trim()));
      setMessage(null);
    } catch (err) {
      setMessage({ text: errorMessage(err), kind: 'error' });
      focus('tv-id');
    } finally {
      setBusy(false);
    }
  };

  useMountEffect(() => {
    if (navState.tranId) void fetchTran(navState.tranId);
  });

  const clear = () => {
    setTranId('');
    setTran(null);
    setMessage(null);
    focus('tv-id');
  };

  return (
    <Screen
      tranId="CT01"
      program="COTRN01C"
      title="View Transaction"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => fetchTran(tranId) },
        { key: 'F3', label: 'Back', action: back },
        { key: 'F4', label: 'Clear', action: clear },
        { key: 'F5', label: 'Browse Tran.', action: () => navigate('/transactions', { state: { from: navState.from } }) },
      ]}
    >
      <div className="form-grid">
        <Field id="tv-id" label="Enter Tran ID:" value={tranId} onChange={setTranId} maxLength={16} autoFocus />
      </div>
      <hr />
      <div className="form-grid cols-3">
        <ReadOnly label="Transaction ID:" value={tran?.tranId ?? ''} width={16} />
        <ReadOnly label="Card Number:" value={tran?.cardNum ?? ''} width={16} />
        <span />
        <ReadOnly label="Type CD:" value={tran?.typeCd ?? ''} width={2} />
        <ReadOnly label="Category CD:" value={tran ? String(tran.catCd).padStart(4, '0') : ''} width={4} />
        <ReadOnly label="Source:" value={tran?.source ?? ''} width={10} />
      </div>
      <div className="form-grid">
        <ReadOnly label="Description:" value={tran?.description ?? ''} width={60} />
      </div>
      <div className="form-grid cols-3">
        <ReadOnly label="Amount:" value={tran?.amt ?? ''} width={12} />
        <ReadOnly label="Orig Date:" value={tran?.origTs.slice(0, 10) ?? ''} width={10} />
        <ReadOnly label="Proc Date:" value={tran?.procTs.slice(0, 10) ?? ''} width={10} />
        <ReadOnly label="Merchant ID:" value={tran ? String(tran.merchantId).padStart(9, '0') : ''} width={9} />
        <ReadOnly label="Merchant Name:" value={tran?.merchantName ?? ''} width={30} />
        <span />
        <ReadOnly label="Merchant City:" value={tran?.merchantCity ?? ''} width={25} />
        <ReadOnly label="Merchant Zip:" value={tran?.merchantZip ?? ''} width={10} />
      </div>
    </Screen>
  );
}
