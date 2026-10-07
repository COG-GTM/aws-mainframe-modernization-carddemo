import { useState } from 'react';
import { api } from '../api/endpoints';
import type { TransactionDetailScreen, TransactionFields } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { transfer, useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COTRN01C');

/** COTRN01 / COTRN1A — COTRN01C (CT01). */
export function TransactionViewPage() {
  const { back, go, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const fromProgram = handed?.context?.fromProgram ?? null;
  const [tranIdIn, setTranIdIn] = useState(handed?.tranId ?? '');
  const [detail, setDetail] = useState<TransactionDetailScreen | null>(null);
  const [busy, setBusy] = useState(false);

  const load = async (id: string = tranIdIn) => {
    setBusy(true);
    try {
      const result = await api.transaction(id.trim() || ' ', fromProgram);
      setDetail(result);
      msg.say(result.message);
    } catch (err) {
      setDetail(null);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed?.tranId) void load(handed.tranId);
  });

  const t: Partial<TransactionFields> = detail?.transaction ?? {};
  const ro = (id: keyof TransactionFields, bms: string, label: string, len: number) => (
    <Field id={`transaction.${id}`} bms={bms} label={label} value={t[id] ?? ''} maxLength={len} readOnly />
  );

  return (
    <Screen
      page={PAGE}
      header={detail?.header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => void load() },
        { key: 'F3', label: 'Back', action: () => back(PAGE, detail?.exit) },
        {
          key: 'F4',
          label: 'Clear',
          action: () => {
            setTranIdIn('');
            setDetail(null);
            msg.clear();
          },
        },
        { key: 'F5', label: 'Browse Tran.', action: () => go(detail?.list ?? transfer(PAGE, 'COTRN00C')) },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="tranId" bms="TRNIDIN" label="Enter Tran ID" value={tranIdIn} onChange={setTranIdIn} maxLength={16} autoFocus invalid={msg.isInvalid('tranId')} />
      </div>
      <div className="form-grid">
        {ro('tranId', 'TRNID', 'Transaction ID', 16)}
        {ro('cardNumber', 'CARDNUM', 'Card Number', 16)}
        {ro('typeCode', 'TTYPCD', 'Type CD', 2)}
        {ro('categoryCode', 'TCATCD', 'Category CD', 4)}
        {ro('source', 'TRNSRC', 'Source', 10)}
        {ro('description', 'TDESC', 'Description', 60)}
        {ro('amount', 'TRNAMT', 'Amount', 12)}
        {ro('origTimestamp', 'TORIGDT', 'Orig Date', 10)}
        {ro('procTimestamp', 'TPROCDT', 'Proc Date', 10)}
        {ro('merchantId', 'MID', 'Merchant ID', 9)}
        {ro('merchantName', 'MNAME', 'Merchant Name', 30)}
        {ro('merchantCity', 'MCITY', 'Merchant City', 25)}
        {ro('merchantZip', 'MZIP', 'Merchant Zip', 10)}
      </div>
    </Screen>
  );
}
