import { useState } from 'react';
import { ApiError, errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { Card } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack, useNavState } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { useMountEffect } from '../lib/useMountEffect';
import { blank } from '../validation/common';
import { validateCardEdit, validateCardKeys, type CardEditForm, type CardField } from '../validation/card';

type Phase = 'search' | 'edit' | 'validated';
const INFO: Record<Phase, string> = {
  search: 'Please enter Account and Card Number',
  edit: 'Update card details presented above.',
  validated: 'Changes validated.Press F5 to save',
};

const toEdit = (c: Card): CardEditForm => {
  const [expYear = '', expMon = ''] = c.expirationDate.split('-');
  return { embossedName: c.embossedName, activeStatus: c.activeStatus, expMon, expYear };
};

const sameEdit = (a: CardEditForm, b: CardEditForm) =>
  a.embossedName.trim().toUpperCase() === b.embossedName.trim().toUpperCase() &&
  a.activeStatus.trim().toUpperCase() === b.activeStatus.trim().toUpperCase() &&
  Number(a.expMon) === Number(b.expMon) &&
  a.expYear.trim() === b.expYear.trim();

export function CardUpdateScreen() {
  const back = useBack();
  const focus = useFieldFocus();
  const navState = useNavState() as { acctId?: string; cardNum?: string; from?: string };
  const fromList = navState.from === '/cards';
  const [acctId, setAcctId] = useState(navState.acctId ?? '');
  const [cardNum, setCardNum] = useState(navState.cardNum ?? '');
  const [card, setCard] = useState<Card | null>(null);
  const [form, setForm] = useState<CardEditForm | null>(null);
  const [phase, setPhase] = useState<Phase>('search');
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<CardField | null>(null);
  const [busy, setBusy] = useState(false);

  const error = (field: CardField | null, text: string) => {
    setInvalid(field);
    setMessage({ text, kind: 'error' });
    if (field) focus(`cu-${field}`);
  };

  const fetchCard = async (a: string, c: string) => {
    const keyError = validateCardKeys(a, c);
    if (keyError) return error(keyError.field, keyError.message);
    setBusy(true);
    try {
      const fetched = await api.getCard(c, a);
      setCard(fetched);
      setForm(toEdit(fetched));
      setPhase('edit');
      setInvalid(null);
      setMessage(null);
      focus('cu-embossedName');
    } catch (err) {
      setCard(null);
      setForm(null);
      setPhase('search');
      error(null, errorMessage(err));
    } finally {
      setBusy(false);
    }
  };

  useMountEffect(() => {
    if (navState.acctId && navState.cardNum) void fetchCard(navState.acctId, navState.cardNum);
  });

  const validate = (): boolean => {
    if (!card || !form) return false;
    if (sameEdit(form, toEdit(card))) {
      setPhase('edit');
      setMessage({ text: 'No change detected with respect to values fetched.', kind: 'info' });
      return false;
    }
    const problem = validateCardEdit(form);
    if (problem) {
      setPhase('edit');
      error(problem.field, problem.message);
      return false;
    }
    setInvalid(null);
    setMessage(null);
    setPhase('validated');
    return true;
  };

  const enter = () => {
    if (!card || card.cardNum !== cardNum || String(card.acctId).padStart(11, '0') !== acctId.padStart(11, '0')) {
      return void fetchCard(acctId, cardNum);
    }
    validate();
  };

  const save = async () => {
    if (!card || !form) return;
    if (phase !== 'validated' && !validate()) return;
    setBusy(true);
    try {
      const saved = await api.updateCard(card.cardNum, {
        acctId: card.acctId,
        embossedName: form.embossedName.trim().toUpperCase(),
        activeStatus: form.activeStatus.trim().toUpperCase(),
        expirationDate: `${form.expYear.trim()}-${form.expMon.trim().padStart(2, '0')}-${
          card.expirationDate.split('-')[2] ?? '01'
        }`,
        version: card.version,
      });
      setCard(saved);
      setForm(toEdit(saved));
      setPhase('edit');
      setMessage({ text: 'Changes committed to database', kind: 'success' });
    } catch (err) {
      setPhase('edit');
      if (err instanceof ApiError && err.status === 409) {
        setMessage({ text: err.message, kind: 'error' });
        try {
          const fresh = await api.getCard(card.cardNum, String(card.acctId).padStart(11, '0'));
          setCard(fresh);
          setForm(toEdit(fresh));
        } catch {
          // keep the user's image when the refresh fails
        }
      } else {
        setMessage({ text: errorMessage(err) || 'Changes unsuccessful. Please try again', kind: 'error' });
      }
    } finally {
      setBusy(false);
    }
  };

  const cancel = () => {
    if (card) setForm(toEdit(card));
    setPhase(card ? 'edit' : 'search');
    setInvalid(null);
    setMessage(null);
  };

  const set = (field: keyof CardEditForm) => (value: string) => {
    setForm((f) => (f ? { ...f, [field]: value } : f));
    if (phase === 'validated') setPhase('edit');
  };

  return (
    <Screen
      tranId="CCUP"
      program="COCRDUPC"
      title="Update Credit Card Details"
      message={message}
      info={INFO[phase]}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Process', action: enter },
        { key: 'F3', label: 'Exit', action: back },
        { key: 'F5', label: 'Save', action: save, disabled: !card },
        { key: 'F12', label: 'Cancel', action: cancel, disabled: !card },
      ]}
    >
      <div className="form-grid">
        <Field
          id="cu-acctId"
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
          id="cu-cardNum"
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
        <Field
          id="cu-embossedName"
          label="Name on card :"
          value={form?.embossedName ?? ''}
          onChange={set('embossedName')}
          maxLength={50}
          readOnly={!form}
          invalid={invalid === 'embossedName'}
        />
        <Field
          id="cu-activeStatus"
          label="Card Active Y/N :"
          value={form?.activeStatus ?? ''}
          onChange={set('activeStatus')}
          maxLength={1}
          readOnly={!form}
          upper
          invalid={invalid === 'activeStatus'}
        />
        <fieldset className="field date-group">
          <legend>Expiry Date :</legend>
          <input
            id="cu-expMon"
            aria-label="Expiry month"
            className="w2"
            value={form?.expMon ?? ''}
            onChange={(e) => set('expMon')(e.target.value)}
            maxLength={2}
            readOnly={!form}
            aria-invalid={invalid === 'expMon' || undefined}
          />
          /
          <input
            id="cu-expYear"
            aria-label="Expiry year"
            className="w4"
            value={form?.expYear ?? ''}
            onChange={(e) => set('expYear')(e.target.value)}
            maxLength={4}
            readOnly={!form}
            aria-invalid={invalid === 'expYear' || undefined}
          />
        </fieldset>
      </div>
      {blank(acctId) && blank(cardNum) && !card && <p className="hint-line">Select a card from the Credit Card List or key it above.</p>}
    </Screen>
  );
}
