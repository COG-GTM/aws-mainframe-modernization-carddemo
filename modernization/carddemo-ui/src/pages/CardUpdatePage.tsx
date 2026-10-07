import { useState } from 'react';
import { ApiError } from '../api/client';
import { api } from '../api/endpoints';
import type { CardDetailScreen, CardUpdateRequest, UpdateState } from '../api/types';
import { Field, FieldGroup, Part } from '../components/Field';
import { Screen, type PfKey } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COCRDUPC');

/** COCRDUP / CCRDUPA — COCRDUPC (CCUP). ENTER = PUT confirm=false, F5 = PUT confirm=true, F12 = re-fetch. */
export function CardUpdatePage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const [accountId, setAccountId] = useState(handed?.context?.acctId ? String(handed.context.acctId).padStart(11, '0') : '');
  const [cardNumber, setCardNumber] = useState('');
  const [card, setCard] = useState<CardDetailScreen | null>(null);
  const [form, setForm] = useState<CardUpdateRequest | null>(null);
  const [state, setState] = useState<UpdateState | null>(null);
  const [info, setInfo] = useState('Please enter Account and Card Number');
  const [busy, setBusy] = useState(false);
  const fromProgram = handed?.context?.fromProgram ?? null;

  const show = (result: CardDetailScreen, infoText?: string) => {
    setCard(result);
    setForm(result.updateForm);
    setAccountId(result.accountId);
    setCardNumber(result.cardNumber);
    setState('SHOW');
    setInfo(infoText ?? 'Details of selected card shown above');
  };

  const fetch = async (key: string = card?.cardRef ?? cardNumber, acct: string = accountId) => {
    setBusy(true);
    try {
      const result = await api.card(key.trim() || ' ', acct.trim(), fromProgram);
      show(result);
      msg.say(result.message);
    } catch (err) {
      setCard(null);
      setForm(null);
      setState(null);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed?.cardRef) void fetch(handed.cardRef, accountId);
  });

  const submit = async (confirm: boolean) => {
    if (!card || !form) return fetch();
    setBusy(true);
    try {
      const result = await api.updateCard(card.cardRef, { ...form, confirm }, fromProgram);
      setState(result.state);
      setInfo(result.infoMessage);
      if (result.state === 'COMMITTED' && result.card) {
        setCard(result.card);
        setForm(result.card.updateForm);
      }
      msg.say(result.message, result.state === 'COMMITTED' ? 'success' : 'info');
    } catch (err) {
      if (err instanceof ApiError && err.status === 409) {
        try {
          show(await api.card(card.cardRef, card.accountId, fromProgram));
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

  const edit = (key: keyof CardUpdateRequest) => (value: string) => {
    if (!form) return;
    setForm({ ...form, [key]: value });
    if (state !== 'SHOW') setState('SHOW');
  };

  const fetched = card !== null;
  const editable = fetched && (state === 'SHOW' || state === 'VALIDATED');
  const pfKeys: PfKey[] = [
    { key: 'ENTER', label: 'Process', action: () => void (fetched ? submit(false) : fetch(cardNumber)) },
    { key: 'F3', label: 'Exit', action: () => back(PAGE, card?.exit) },
  ];
  if (state === 'VALIDATED') pfKeys.push({ key: 'F5', label: 'Save', action: () => void submit(true) });
  if (fetched) pfKeys.push({ key: 'F12', label: 'Cancel', action: () => void fetch(card.cardRef, card.accountId) });

  return (
    <Screen page={PAGE} header={card?.header} message={msg.message} info={info} busy={busy} pfKeys={pfKeys}>
      <div className="form-grid narrow">
        <Field id="accountId" bms="ACCTSID" label="Account Number" value={accountId} onChange={setAccountId} maxLength={11} readOnly={fetched} autoFocus invalid={msg.isInvalid('accountId')} />
        <Field id="cardNumber" bms="CARDSID" label="Card Number" value={cardNumber} onChange={setCardNumber} maxLength={16} readOnly={fetched} invalid={msg.isInvalid('cardNumber')} />
      </div>
      <div className="form-grid narrow">
        <Field id="embossedName" bms="CRDNAME" label="Name on card" value={form?.embossedName ?? ''} onChange={edit('embossedName')} maxLength={50} readOnly={!editable} invalid={msg.isInvalid('embossedName')} />
        <Field id="activeStatus" bms="CRDSTCD" label="Card Active Y/N" value={form?.activeStatus ?? ''} onChange={edit('activeStatus')} maxLength={1} upper readOnly={!editable} invalid={msg.isInvalid('activeStatus')} />
        <FieldGroup label="Expiry Date" htmlFor="expiryMonth">
          <Part id="expiryMonth" bms="EXPMON" label="Expiry month" value={form?.expiryMonth ?? ''} onChange={edit('expiryMonth')} maxLength={2} readOnly={!editable} invalid={msg.isInvalid('expiryMonth')} />
          <span>/</span>
          <Part id="expiryYear" bms="EXPYEAR" label="Expiry year" value={form?.expiryYear ?? ''} onChange={edit('expiryYear')} maxLength={4} readOnly={!editable} invalid={msg.isInvalid('expiryYear')} />
          {/* EXPDAY: DRK,PROT on the map — carried, never shown */}
          <input type="hidden" data-bms="EXPDAY" value="" />
        </FieldGroup>
      </div>
    </Screen>
  );
}
