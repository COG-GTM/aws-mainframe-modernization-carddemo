import { useState } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import { ApiError, errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { User, UserType } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { useBack } from '../lib/navigation';
import { useFieldFocus } from '../lib/useFieldFocus';
import { useMountEffect } from '../lib/useMountEffect';
import { blank } from '../validation/common';
import { EMPTY_USER_FORM, validateUserUpdate, type UserField, type UserForm } from '../validation/user';

const toForm = (u: User): UserForm => ({ userId: u.userId, firstName: u.firstName, lastName: u.lastName, password: '', userType: u.userType });

export function UserUpdateScreen() {
  const params = useParams();
  const back = useBack('/admin');
  const navigate = useNavigate();
  const focus = useFieldFocus();
  const [user, setUser] = useState<User | null>(null);
  const [form, setForm] = useState<UserForm>({ ...EMPTY_USER_FORM, userId: params.userId ?? '' });
  const [message, setMessage] = useState<ScreenMessage | null>(null);
  const [invalid, setInvalid] = useState<UserField | null>(null);
  const [busy, setBusy] = useState(false);

  const error = (field: UserField | null, text: string, kind: ScreenMessage['kind'] = 'error') => {
    setInvalid(kind === 'error' ? field : null);
    setMessage({ text, kind });
    if (field) focus(`uu-${field}`);
  };

  const fetchUser = async (userId: string) => {
    if (blank(userId)) return error('userId', 'User ID can NOT be empty...');
    setBusy(true);
    try {
      const fetched = await api.getUser(userId.trim().toUpperCase());
      setUser(fetched);
      setForm(toForm(fetched));
      error('firstName', 'Press PF5 key to save your updates ...', 'info');
    } catch (err) {
      setUser(null);
      error('userId', errorMessage(err));
    } finally {
      setBusy(false);
    }
  };

  useMountEffect(() => {
    if (params.userId) void fetchUser(params.userId);
  });

  /** COUSR02C UPDATE-USER-INFO; returns true when the record was saved. */
  const save = async (): Promise<boolean> => {
    const problem = validateUserUpdate(form);
    if (problem) {
      error(problem.field, problem.message);
      return false;
    }
    if (!user || user.userId !== form.userId.trim().toUpperCase()) {
      await fetchUser(form.userId);
      return false;
    }
    const unchanged =
      form.firstName.trim() === user.firstName &&
      form.lastName.trim() === user.lastName &&
      form.userType.trim().toUpperCase() === user.userType &&
      blank(form.password);
    if (unchanged) {
      error('firstName', 'Please modify to update ...');
      return false;
    }
    setBusy(true);
    try {
      const saved = await api.updateUser(user.userId, {
        firstName: form.firstName.trim(),
        lastName: form.lastName.trim(),
        userType: form.userType.trim().toUpperCase() as UserType,
        password: blank(form.password) ? undefined : form.password,
        version: user.version,
      });
      setUser(saved);
      setForm(toForm(saved));
      setInvalid(null);
      setMessage({ text: `User ${saved.userId} has been updated ...`, kind: 'success' });
      return true;
    } catch (err) {
      error(null, err instanceof ApiError && err.status >= 500 ? 'Unable to Update User...' : errorMessage(err));
      return false;
    } finally {
      setBusy(false);
    }
  };

  const clear = () => {
    setUser(null);
    setForm(EMPTY_USER_FORM);
    setInvalid(null);
    setMessage(null);
    focus('uu-userId');
  };

  const saveAndExit = async () => {
    if (!user || (await save())) back();
  };

  const set = (field: UserField) => (value: string) => setForm((f) => ({ ...f, [field]: value }));

  return (
    <Screen
      tranId="CU02"
      program="COUSR02C"
      title="Update User"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Fetch', action: () => fetchUser(form.userId) },
        { key: 'F3', label: 'Save&Exit', action: saveAndExit },
        { key: 'F4', label: 'Clear', action: clear },
        { key: 'F5', label: 'Save', action: () => void save() },
        { key: 'F12', label: 'Cancel', action: () => navigate('/admin') },
      ]}
    >
      <div className="form-grid">
        <Field id="uu-userId" label="Enter User ID:" value={form.userId} onChange={set('userId')} maxLength={8} upper autoFocus={!params.userId} invalid={invalid === 'userId'} />
      </div>
      <hr />
      <div className="form-grid cols-2">
        <Field id="uu-firstName" label="First Name:" value={form.firstName} onChange={set('firstName')} maxLength={20} invalid={invalid === 'firstName'} />
        <Field id="uu-lastName" label="Last Name:" value={form.lastName} onChange={set('lastName')} maxLength={20} invalid={invalid === 'lastName'} />
        <Field
          id="uu-password"
          label="Password:"
          type="password"
          value={form.password}
          onChange={set('password')}
          maxLength={8}
          hint="(8 Char, blank = unchanged)"
          invalid={invalid === 'password'}
        />
        <Field id="uu-userType" label="User Type:" value={form.userType} onChange={set('userType')} maxLength={1} hint="(A=Admin, U=User)" upper invalid={invalid === 'userType'} />
      </div>
    </Screen>
  );
}
