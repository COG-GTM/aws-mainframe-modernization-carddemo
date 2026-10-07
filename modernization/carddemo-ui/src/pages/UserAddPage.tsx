import { useState } from 'react';
import { api } from '../api/endpoints';
import type { NavigationContext, ScreenHeader, UserAddRequest } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { transfer, useProgramNav } from '../lib/navigation';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COUSR01C');
const EMPTY: UserAddRequest = { firstName: '', lastName: '', userId: '', password: '', userType: '' };

/** COUSR01 / COUSR1A — COUSR01C (CU01). */
export function UserAddPage() {
  const { back, go } = useProgramNav();
  const msg = useMessages();
  const [form, setForm] = useState<UserAddRequest>(EMPTY);
  const [header, setHeader] = useState<ScreenHeader | null>(null);
  const [exit, setExit] = useState<NavigationContext | null>(null);
  const [busy, setBusy] = useState(false);

  const add = async () => {
    setBusy(true);
    try {
      const result = await api.addUser(form);
      setHeader(result.header);
      setExit(result.exit);
      setForm(EMPTY);
      msg.say(result.message, 'success');
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  const f = (id: keyof UserAddRequest, bms: string, label: string, len: number, opts: { upper?: boolean; dark?: boolean; hint?: string } = {}) => (
    <Field id={id} bms={bms} label={label} value={form[id]} onChange={(v) => setForm((c) => ({ ...c, [id]: v }))} maxLength={len} invalid={msg.isInvalid(id)} {...opts} />
  );

  return (
    <Screen
      page={PAGE}
      header={header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Add User', action: () => void add() },
        { key: 'F3', label: 'Back', action: () => back(PAGE, exit) },
        {
          key: 'F4',
          label: 'Clear',
          action: () => {
            setForm(EMPTY);
            msg.clear();
          },
        },
        { key: 'F12', label: 'Exit', action: () => go(transfer(PAGE, 'COADM01C')) },
      ]}
    >
      <div className="form-grid">
        {f('firstName', 'FNAME', 'First Name', 20)}
        {f('lastName', 'LNAME', 'Last Name', 20)}
        {f('userId', 'USERID', 'User ID', 8, { upper: true, hint: '(8 Char)' })}
        {f('password', 'PASSWD', 'Password', 8, { dark: true, hint: '(8 Char)' })}
        {f('userType', 'USRTYPE', 'User Type', 1, { upper: true, hint: '(A=Admin, U=User)' })}
      </div>
    </Screen>
  );
}
