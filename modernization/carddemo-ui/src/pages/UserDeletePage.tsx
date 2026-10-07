import { useState } from 'react';
import { api } from '../api/endpoints';
import type { UserScreen } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useOnMount } from '../lib/useOnMount';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';

const PAGE = page('COUSR03C');

/** COUSR03 / COUSR3A — COUSR03C (CU03). ENTER = fetch (DELETE without confirm), F5 = DELETE confirm=Y&version. */
export function UserDeletePage() {
  const { back, incoming } = useProgramNav();
  const msg = useMessages();
  const handed = incoming(PAGE.program);
  const fromProgram = handed?.context?.fromProgram ?? null;
  const [userId, setUserId] = useState(handed?.userId ?? '');
  const [screen, setScreen] = useState<UserScreen | null>(null);
  const [busy, setBusy] = useState(false);

  const call = async (confirm: string, id: string = userId) => {
    setBusy(true);
    try {
      const version = confirm === 'Y' ? (screen?.user?.version ?? null) : null;
      const result = await api.deleteUser(id.trim() || ' ', confirm, version, fromProgram);
      setScreen(result.state === 'DELETED' ? { ...result, user: null } : result);
      if (result.state === 'DELETED') setUserId('');
      msg.say(result.message, result.state === 'DELETED' ? 'success' : 'info');
    } catch (err) {
      setScreen(null);
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  useOnMount(() => {
    if (handed?.userId) void call('', handed.userId);
  });

  const changeUserId = (value: string) => {
    setUserId(value);
    if (screen?.user && value.trim() !== screen.user.userId) setScreen(null);
  };

  const u = screen?.user;
  return (
    <Screen
      page={PAGE}
      header={screen?.header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => void call('') },
        { key: 'F3', label: 'Back', action: () => back(PAGE, screen?.exit) },
        {
          key: 'F4',
          label: 'Clear',
          action: () => {
            setUserId('');
            setScreen(null);
            msg.clear();
          },
        },
        { key: 'F5', label: 'Delete', action: () => void call('Y') },
      ]}
    >
      <div className="form-grid narrow">
        <Field id="userId" bms="USRIDIN" label="Enter User ID" value={userId} onChange={changeUserId} maxLength={8} upper autoFocus invalid={msg.isInvalid('userId')} />
      </div>
      <div className="form-grid">
        <Field id="firstName" bms="FNAME" label="First Name" value={u?.firstName ?? ''} maxLength={20} readOnly />
        <Field id="lastName" bms="LNAME" label="Last Name" value={u?.lastName ?? ''} maxLength={20} readOnly />
        <Field id="userType" bms="USRTYPE" label="User Type" value={u?.userType ?? ''} maxLength={1} readOnly hint="(A=Admin, U=User)" />
      </div>
    </Screen>
  );
}
