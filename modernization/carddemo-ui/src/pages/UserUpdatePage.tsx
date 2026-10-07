import { useState } from 'react';
import { api } from '../api/endpoints';
import type { UserScreen, UserUpdateRequest } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { transfer, useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COUSR02C');
type Form = Omit<UserUpdateRequest, 'version'>;
const EMPTY: Form = { firstName: '', lastName: '', password: '', userType: '' };

/** COUSR02 / COUSR2A — COUSR02C (CU02). ENTER = fetch, F5 = save, F3 = save & exit, F12 = cancel. */
export function UserUpdatePage() {
  const { back, go, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const fromProgram = handed?.context?.fromProgram ?? null;
  const [userId, setUserId] = useState(handed?.userId ?? '');
  const [screen, setScreen] = useState<UserScreen | null>(null);
  const [form, setForm] = useState<Form>(EMPTY);
  const [busy, setBusy] = useState(false);

  const fetch = async (id: string = userId) => {
    setBusy(true);
    try {
      const result = await api.user(id.trim() || ' ', fromProgram);
      setScreen(result);
      if (result.user) {
        setUserId(result.user.userId);
        setForm({
          firstName: result.user.firstName,
          lastName: result.user.lastName,
          password: result.user.password ?? '',
          userType: result.user.userType,
        });
      }
      msg.say(result.message);
    } catch (err) {
      setScreen(null);
      setForm(EMPTY);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed?.userId) void fetch(handed.userId);
  });

  const save = async (thenExit: boolean) => {
    if (!screen?.user) {
      if (thenExit) back(PAGE, screen?.exit);
      else void fetch();
      return;
    }
    setBusy(true);
    try {
      const result = await api.updateUser(screen.user.userId, { ...form, version: screen.user.version }, fromProgram);
      setScreen(result);
      msg.say(result.message, result.state === 'UPDATED' ? 'success' : 'info');
      if (thenExit) back(PAGE, result.exit);
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  const changeUserId = (value: string) => {
    setUserId(value);
    if (screen?.user && value.trim() !== screen.user.userId) {
      setScreen(null);
      setForm(EMPTY);
    }
  };

  const f = (id: keyof Form, bms: string, label: string, len: number, opts: { upper?: boolean; dark?: boolean; hint?: string } = {}) => (
    <Field id={id} bms={bms} label={label} value={form[id]} onChange={(v) => setForm((c) => ({ ...c, [id]: v }))} maxLength={len} invalid={msg.isInvalid(id)} {...opts} />
  );

  return (
    <Screen
      page={PAGE}
      header={screen?.header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => void fetch() },
        { key: 'F3', label: 'Save&Exit', action: () => void save(true) },
        {
          key: 'F4',
          label: 'Clear',
          action: () => {
            setUserId('');
            setScreen(null);
            setForm(EMPTY);
            msg.clear();
          },
        },
        { key: 'F5', label: 'Save', action: () => void save(false) },
        { key: 'F12', label: 'Cancel', action: () => go(transfer(PAGE, 'COADM01C')) },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="userId" bms="USRIDIN" label="Enter User ID" value={userId} onChange={changeUserId} maxLength={8} upper autoFocus invalid={msg.isInvalid('userId')} />
      </div>
      <div className="form-grid">
        {f('firstName', 'FNAME', 'First Name', 20)}
        {f('lastName', 'LNAME', 'Last Name', 20)}
        {f('password', 'PASSWD', 'Password', 8, { dark: true, hint: '(8 Char)' })}
        {f('userType', 'USRTYPE', 'User Type', 1, { upper: true, hint: '(A=Admin, U=User)' })}
      </div>
    </Screen>
  );
}
