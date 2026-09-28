import { useState } from 'react';
import { useParams } from 'react-router-dom';
import { ApiError, errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { User } from '../api/types';
import { Field, ReadOnly } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { useMountEffect } from '../lib/useMountEffect';
import { blank } from '../validation/common';

export function UserDeleteScreen() {
  const params = useParams();
  const back = useBack('/admin');
  const focus = useFieldFocus();
  const [userId, setUserId] = useState(params.userId ?? '');
  const [user, setUser] = useState<User | null>(null);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [busy, setBusy] = useState(false);

  const fetchUser = async (id: string) => {
    setUser(null);
    if (blank(id)) {
      focus('ud-userId');
      return setMessage({ text: 'User ID can NOT be empty...', kind: 'error' });
    }
    setBusy(true);
    try {
      setUser(await api.getUser(id.trim().toUpperCase()));
      setMessage({ text: 'Press PF5 key to delete this user ...', kind: 'info' });
    } catch (err) {
      setMessage({ text: errorMessage(err), kind: 'error' });
      focus('ud-userId');
    } finally {
      setBusy(false);
    }
  };

  useMountEffect(() => {
    if (params.userId) void fetchUser(params.userId);
  });

  const remove = async () => {
    if (blank(userId)) return setMessage({ text: 'User ID can NOT be empty...', kind: 'error' });
    if (!user || user.userId !== userId.trim().toUpperCase()) return fetchUser(userId);
    setBusy(true);
    try {
      await api.deleteUser(user.userId);
      setUser(null);
      setUserId('');
      setMessage({ text: `User ${user.userId} has been deleted ...`, kind: 'success' });
      focus('ud-userId');
    } catch (err) {
      setMessage({
        text: err instanceof ApiError && err.status >= 500 ? 'Unable to Update User...' : errorMessage(err),
        kind: 'error',
      });
    } finally {
      setBusy(false);
    }
  };

  const clear = () => {
    setUserId('');
    setUser(null);
    setMessage(null);
    focus('ud-userId');
  };

  return (
    <Screen
      tranId="CU03"
      program="COUSR03C"
      title="Delete User"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => fetchUser(userId) },
        { key: 'F3', label: 'Back', action: back },
        { key: 'F4', label: 'Clear', action: clear },
        { key: 'F5', label: 'Delete', action: remove },
      ]}
    >
      <div className="form-grid">
        <Field id="ud-userId" label="Enter User ID:" value={userId} onChange={setUserId} maxLength={8} upper autoFocus={!params.userId} />
      </div>
      <hr />
      <div className="form-grid cols-2">
        <ReadOnly label="First Name:" value={user?.firstName ?? ''} width={20} />
        <ReadOnly label="Last Name:" value={user?.lastName ?? ''} width={20} />
        <ReadOnly label="User Type:" value={user ? `${user.userType}  (A=Admin, U=User)` : ''} width={20} />
      </div>
    </Screen>
  );
}
