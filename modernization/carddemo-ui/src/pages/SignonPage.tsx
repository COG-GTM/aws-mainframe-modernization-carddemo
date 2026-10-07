import { useEffect, useState } from 'react';
import { api } from '../api/endpoints';
import type { ScreenHeader } from '../api/types';
import { Field } from '../components/Field';
import { Screen } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';
import { useSession } from '../session/session';

const PAGE = page('COSGN00C');

const NOTE = [
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

/** COSGN00 / COSGN0A — COSGN00C (CC00). */
export function SignonPage() {
  const { signIn, signOffMessage } = useSession();
  const { go } = useProgramNav();
  const msg = useMessages();
  const [header, setHeader] = useState<ScreenHeader | null>(null);
  const [userId, setUserId] = useState('');
  const [password, setPassword] = useState('');
  const [busy, setBusy] = useState(false);
  const { say } = msg;

  useEffect(() => {
    let live = true;
    api
      .signOnScreen()
      .then((s) => live && setHeader(s.header))
      .catch(() => undefined);
    return () => {
      live = false;
    };
  }, []);

  useEffect(() => {
    if (signOffMessage) say(signOffMessage);
  }, [signOffMessage, say]);

  const signOn = async () => {
    setBusy(true);
    try {
      const login = await api.login(userId, password);
      signIn(login);
      go(login.navigation);
    } catch (err) {
      msg.fail(err);
    } finally {
      setBusy(false);
    }
  };

  const exit = async () => {
    try {
      const bye = await api.logout();
      say(bye.message);
    } catch (err) {
      msg.fail(err);
    }
    setUserId('');
    setPassword('');
  };

  return (
    <Screen
      page={PAGE}
      header={header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Sign-on', action: signOn },
        { key: 'F3', label: 'Exit', action: exit },
      ]}
    >
      <div className="signon-ids">
        <span>
          <span className="hdr-label">AppID:</span> <span data-bms="APPLID">{header?.applId ?? ''}</span>
        </span>
        <span>
          <span className="hdr-label">SysID:</span> <span data-bms="SYSID">{header?.sysId ?? ''}</span>
        </span>
      </div>
      <p className="lead">This is a Credit Card Demo Application for Mainframe Modernization</p>
      <pre className="banner" aria-hidden="true">
        {NOTE}
      </pre>
      <p className="lead">Type your User ID and Password, then press ENTER:</p>
      <div className="form-grid narrow">
        <Field id="userId" bms="USERID" label="User ID" value={userId} onChange={setUserId} maxLength={8} upper hint="(8 Char)" autoFocus invalid={msg.isInvalid('userId')} />
        <Field id="password" bms="PASSWD" label="Password" value={password} onChange={setPassword} maxLength={8} dark hint="(8 Char)" invalid={msg.isInvalid('password')} />
      </div>
    </Screen>
  );
}
