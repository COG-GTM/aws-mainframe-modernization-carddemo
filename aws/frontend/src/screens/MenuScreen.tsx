import { useEffect, useRef, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { errorMessage } from '../api/client';
import { api } from '../api/endpoints';
import type { MenuOption } from '../api/types';
import { ADMIN_ONLY_MESSAGE } from '../auth/RequireAuth';
import { useAuth } from '../auth/context';
import { Field } from '../components/Field';
import { Screen, type ScreenMessage } from '../components/Screen';
import { THANK_YOU } from '../config';
import { useNavState } from '../lib/navigation';
import { IMPLEMENTED_ROUTES, menuTarget } from '../routes';

interface MenuScreenProps {
  kind: 'main' | 'admin';
}

const CONFIG = {
  main: { tranId: 'CM00', program: 'COMEN01C', title: 'Main Menu', notInstalled: ' is not installed...' },
  admin: { tranId: 'CA00', program: 'COADM01C', title: 'Admin Menu', notInstalled: ' is not installed ...' },
} as const;

export function MenuScreen({ kind }: MenuScreenProps) {
  const cfg = CONFIG[kind];
  const navigate = useNavigate();
  const navState = useNavState();
  const { session, signOut } = useAuth();
  const [options, setOptions] = useState<MenuOption[]>([]);
  const [option, setOption] = useState('');
  const [message, setMessage] = useState<ScreenMessage | null>(
    navState.message ? { text: navState.message, kind: 'error' } : null,
  );
  const [busy, setBusy] = useState(true);
  const optionRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    let active = true;
    (kind === 'admin' ? api.adminMenu() : api.mainMenu())
      .then((res) => active && setOptions(res.options))
      .catch((err) => active && setMessage({ text: errorMessage(err), kind: 'error' }))
      .finally(() => active && setBusy(false));
    return () => {
      active = false;
    };
  }, [kind]);

  const fail = (text: string) => {
    setMessage({ text, kind: 'error' });
    optionRef.current?.focus();
  };

  const select = (value: string) => {
    const trimmed = value.trim();
    const number = Number(trimmed);
    const chosen = /^\d{1,2}$/.test(trimmed) ? options.find((o) => o.number === number) : undefined;
    if (!chosen || number === 0) return fail('Please enter a valid option number...');
    if (chosen.adminOnly && session?.role !== 'ADMIN') return fail(ADMIN_ONLY_MESSAGE);
    const target = menuTarget(chosen.route);
    if (!chosen.installed || !IMPLEMENTED_ROUTES.has(target)) {
      return setMessage({ text: `This option ${chosen.name}${cfg.notInstalled}`, kind: 'info' });
    }
    navigate(target, { state: { from: kind === 'admin' ? '/admin' : '/menu' } });
  };

  const exit = () => {
    signOut();
    navigate('/login', { replace: true, state: { message: THANK_YOU } });
  };

  return (
    <Screen
      tranId={cfg.tranId}
      program={cfg.program}
      title={cfg.title}
      message={message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => select(option) },
        { key: 'F3', label: 'Exit', action: exit },
      ]}
    >
      <ol className="menu-list" aria-label={cfg.title}>
        {options.map((o) => {
          const available = o.installed && IMPLEMENTED_ROUTES.has(menuTarget(o.route));
          return (
            <li key={o.number}>
              <button
                type="button"
                className={`menu-option ${available ? '' : 'menu-option-unavailable'}`}
                onClick={() => {
                  setOption(String(o.number));
                  select(String(o.number));
                }}
              >
                <span className="menu-num">{String(o.number).padStart(2, ' ')}.</span> {o.name}
                {!available && <span className="badge">not installed</span>}
              </button>
            </li>
          );
        })}
      </ol>
      <div className="form-grid narrow">
        <Field
          ref={optionRef}
          id="menu-option"
          label="Please select an option :"
          value={option}
          onChange={(v) => setOption(v.replace(/\D/g, ''))}
          maxLength={2}
          inputMode="numeric"
          autoFocus
        />
      </div>
    </Screen>
  );
}
