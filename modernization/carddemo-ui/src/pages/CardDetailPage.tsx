import { useState } from 'react';
import { api } from '../api/endpoints';
import type { CardDetailScreen } from '../api/types';
import { Field, FieldGroup, Part } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COCRDSLC');

/** COCRDSL / CCRDSLA — COCRDSLC (CCDL). A card selected on the list arrives as its opaque cardRef. */
export function CardDetailPage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const [accountId, setAccountId] = useState(handed?.context?.acctId ? String(handed.context.acctId).padStart(11, '0') : '');
  const [cardNumber, setCardNumber] = useState('');
  const [card, setCard] = useState<CardDetailScreen | null>(null);
  const [busy, setBusy] = useState(false);
  const fromProgram = handed?.context?.fromProgram ?? null;

  const load = async (key: string = cardNumber, acct: string = accountId) => {
    setBusy(true);
    try {
      const result = await api.card(key.trim() || ' ', acct.trim(), fromProgram);
      setCard(result);
      setAccountId(result.accountId);
      setCardNumber(result.cardNumber);
      msg.say(result.message);
    } catch (err) {
      setCard(null);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed?.cardRef) void load(handed.cardRef, accountId);
  });

  return (
    <Screen
      page={PAGE}
      header={card?.header}
      message={msg.message}
      info={card ? card.infoMessage : 'Please enter Account and Card Number'}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Search Cards', action: () => void load() },
        { key: 'F3', label: 'Exit', action: () => back(PAGE, card?.exit) },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="accountId" bms="ACCTSID" label="Account Number" value={accountId} onChange={setAccountId} maxLength={11} autoFocus invalid={msg.isInvalid('accountId')} />
        <Field id="cardNumber" bms="CARDSID" label="Card Number" value={cardNumber} onChange={setCardNumber} maxLength={16} invalid={msg.isInvalid('cardNumber')} />
      </div>
      <div className="form-grid narrow">
        <Field id="embossedName" bms="CRDNAME" label="Name on card" value={card?.embossedName ?? ''} maxLength={50} readOnly />
        <Field id="activeStatus" bms="CRDSTCD" label="Card Active Y/N" value={card?.activeStatus ?? ''} maxLength={1} readOnly />
        <FieldGroup label="Expiry Date" htmlFor="expiryMonth">
          <Part id="expiryMonth" bms="EXPMON" label="Expiry month" value={card?.expiryMonth ?? ''} maxLength={2} readOnly />
          <span>/</span>
          <Part id="expiryYear" bms="EXPYEAR" label="Expiry year" value={card?.expiryYear ?? ''} maxLength={4} readOnly />
        </FieldGroup>
      </div>
    </Screen>
  );
}
