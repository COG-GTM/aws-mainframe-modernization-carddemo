import { useRef, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import { homeRoute, useAuth } from '../auth/context';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { THANK_YOU } from '../config';
import { useNavState } from '../lib/navigation';

const BANNER = [
  '+========================================+',
  '|%%%%%%%  NATIONAL RESERVE NOTE  %%%%%%%%|',
  '|%(1)  THE UNITED STATES OF KICSLAND (1)%|',
  '|%$$              ___       ********  $$%|',
  '|%$    {x}       (o o)                 $%|',
  '|%$     ******  (  V  )      O N E     $%|',
  '|%(1)          ---m-m---             (1)%|',
  '|%%~~~~~~~~~~~ ONE DOLLAR ~~~~~~~~~~~~~%%|',
  '+========================================+',
].join('\n');

export function SignonScreen() {
  const navigate = useNavigate();
  const { signIn, signOut } = useAuth();
  const navState = useNavState();
  const [userId, setUserId] = useState('');
  const [password, setPassword] = useState('');
  const [message, setMessage] = useState<ScreenMessage | null>(
    navState.message ? { text: navState.message, kind: 'info' } : null,
  );
  const [invalid, setInvalid] = useState<'userId' | 'password' | null>(null);
  const [busy, setBusy] = useState(false);
  const userRef = useRef<HTMLInputElement>(null);
  const passRef = useRef<HTMLInputElement>(null);

  const fail = (field: 'userId' | 'password' | null, text: string) => {
    setInvalid(field);
    setMessage({ text, kind: 'error' });
    (field === 'password' ? passRef : userRef).current?.focus();
  };

  const submit = async () => {
    if (!userId.trim()) return fail('userId', 'Please enter User ID ...');
    if (!password.trim()) return fail('password', 'Please enter Password ...');
    setBusy(true);
    try {
      const response = await api.signon({ userId: userId.trim().toUpperCase(), password: password.trim().toUpperCase() });
      const session = signIn(response.token);
      navigate(homeRoute(session.role), { replace: true });
    } catch (err) {
      setPassword('');
      fail('password', errorMessage(err));
    } finally {
      setBusy(false);
    }
  };

  const exit = () => {
    signOut();
    setUserId('');
    setPassword('');
    setInvalid(null);
    setMessage({ text: THANK_YOU, kind: 'info' });
  };

  return (
    <Screen
      tranId="CC00"
      program="COSGN00C"
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Sign-on', action: submit },
        { key: 'F3', label: 'Exit', action: exit },
      ]}
    >
      <p className="lead center">This is a Credit Card Demo Application for Mainframe Modernization</p>
      <pre className="banner" aria-hidden="true">
        {BANNER}
      </pre>
      <p className="center">Type your User ID and Password, then press ENTER:</p>
      <div className="form-grid narrow">
        <Field
          ref={userRef}
          label="User ID"
          value={userId}
          onChange={setUserId}
          maxLength={8}
          hint="(8 Char)"
          invalid={invalid === 'userId'}
          autoFocus
          upper
        />
        <Field
          ref={passRef}
          label="Password"
          type="password"
          value={password}
          onChange={setPassword}
          maxLength={8}
          hint="(8 Char)"
          invalid={invalid === 'password'}
        />
      </div>
    </Screen>
  );
}
