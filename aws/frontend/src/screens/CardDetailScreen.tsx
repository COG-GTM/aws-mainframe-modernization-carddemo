import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { Card } from '../api/types';
import { Field, ReadOnly } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack, useNavState } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { useMountEffect } from '../lib/useMountEffect';
import { validateCardKeys } from '../validation/card';

export function CardDetailScreen() {
  const back = useBack();
  const navigate = useNavigate();
  const focus = useFieldFocus();
  const navState = useNavState() as { acctId?: string; cardNum?: string; from?: string };
  const fromList = navState.from === '/cards';
  const [acctId, setAcctId] = useState(navState.acctId ?? '');
  const [cardNum, setCardNum] = useState(navState.cardNum ?? '');
  const [card, setCard] = useState<Card | null>(null);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<'acctId' | 'cardNum' | null>(null);
  const [busy, setBusy] = useState(false);

  const search = async (a: string, c: string) => {
    const keyError = validateCardKeys(a, c);
    if (keyError) {
      setCard(null);
      setInvalid(keyError.field);
      focus(keyError.field === 'acctId' ? 'cs-acct' : 'cs-card');
      return setMessage({ text: keyError.message, kind: 'error' });
    }
    setBusy(true);
    try {
      setCard(await api.getCard(c, a));
      setInvalid(null);
      setMessage(null);
    } catch (err) {
      setCard(null);
      setMessage({ text: errorMessage(err), kind: 'error' });
    } finally {
      setBusy(false);
    }
  };

  useMountEffect(() => {
    if (navState.acctId && navState.cardNum) void search(navState.acctId, navState.cardNum);
  });

  const [expYear = '', expMon = ''] = (card?.expirationDate ?? '').split('-');

  return (
    <Screen
      tranId="CCDL"
      program="COCRDSLC"
      title="View Credit Card Detail"
      message={message}
      info={card ? '   Displaying requested details' : 'Please enter Account and Card Number'}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Search Cards', action: () => search(acctId, cardNum) },
        { key: 'F3', label: 'Exit', action: back },
        ...(card
          ? [
              {
                key: 'F5' as const,
                label: 'Update',
                action: () =>
                  navigate('/cards/update', {
                    state: { from: navState.from, acctId: acctId.padStart(11, '0'), cardNum },
                  }),
              },
            ]
          : []),
      ]}
    >
      <div className="form-grid">
        <Field
          id="cs-acct"
          label="Account Number :"
          value={acctId}
          onChange={(v) => setAcctId(v.replace(/\D/g, ''))}
          maxLength={11}
          inputMode="numeric"
          readOnly={fromList}
          autoFocus={!fromList}
          invalid={invalid === 'acctId'}
        />
        <Field
          id="cs-card"
          label="Card Number :"
          value={cardNum}
          onChange={(v) => setCardNum(v.replace(/\D/g, ''))}
          maxLength={16}
          inputMode="numeric"
          readOnly={fromList}
          invalid={invalid === 'cardNum'}
        />
      </div>
      <div className="form-grid">
        <ReadOnly label="Name on card :" value={card?.embossedName ?? ''} width={50} />
        <ReadOnly label="Card Active Y/N :" value={card?.activeStatus ?? ''} width={1} />
        <ReadOnly label="Expiry Date :" value={card ? `${expMon} / ${expYear}` : ''} width={9} />
      </div>
    </Screen>
  );
}
