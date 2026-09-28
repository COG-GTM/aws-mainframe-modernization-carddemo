import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { UserType } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { EMPTY_USER_FORM, validateUserAdd, type UserField, type UserForm } from '../validation/user';

export function UserAddScreen() {
  const back = useBack('/admin');
  const navigate = useNavigate();
  const focus = useFieldFocus();
  const [form, setForm] = useState<UserForm>(EMPTY_USER_FORM);
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<UserField | null>(null);
  const [busy, setBusy] = useState(false);

  const error = (field: UserField, text: string) => {
    setInvalid(field);
    setMessage({ text, kind: 'error' });
    focus(`ua-${field}`);
  };

  const clear = () => {
    setForm(EMPTY_USER_FORM);
    setInvalid(null);
    setMessage(null);
    focus('ua-firstName');
  };

  const add = async () => {
    const problem = validateUserAdd(form);
    if (problem) return error(problem.field, problem.message);
    setBusy(true);
    try {
      const created = await api.addUser({
        userId: form.userId.trim().toUpperCase(),
        firstName: form.firstName.trim(),
        lastName: form.lastName.trim(),
        password: form.password,
        userType: form.userType.trim().toUpperCase() as UserType,
      });
      setForm(EMPTY_USER_FORM);
      setInvalid(null);
      setMessage({ text: `User ${created.userId} has been added ...`, kind: 'success' });
      focus('ua-firstName');
    } catch (err) {
      error('userId', errorMessage(err));
    } finally {
      setBusy(false);
    }
  };

  const set = (field: UserField) => (value: string) => setForm((f) => ({ ...f, [field]: value }));

  return (
    <Screen
      tranId="CU01"
      program="COUSR01C"
      title="Add User"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Add User', action: add },
        { key: 'F3', label: 'Back', action: back },
        { key: 'F4', label: 'Clear', action: clear },
        { key: 'F12', label: 'Exit', action: () => navigate('/admin') },
      ]}
    >
      <div className="form-grid cols-2">
        <Field id="ua-firstName" label="First Name:" value={form.firstName} onChange={set('firstName')} maxLength={20} autoFocus invalid={invalid === 'firstName'} />
        <Field id="ua-lastName" label="Last Name:" value={form.lastName} onChange={set('lastName')} maxLength={20} invalid={invalid === 'lastName'} />
        <Field id="ua-userId" label="User ID:" value={form.userId} onChange={set('userId')} maxLength={8} hint="(8 Char)" upper invalid={invalid === 'userId'} />
        <Field id="ua-password" label="Password:" type="password" value={form.password} onChange={set('password')} maxLength={8} hint="(8 Char)" invalid={invalid === 'password'} />
        <Field id="ua-userType" label="User Type:" value={form.userType} onChange={set('userType')} maxLength={1} hint="(A=Admin, U=User)" upper invalid={invalid === 'userType'} />
      </div>
    </Screen>
  );
}
