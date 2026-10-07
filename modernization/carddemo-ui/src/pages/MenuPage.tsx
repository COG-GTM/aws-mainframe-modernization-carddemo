import { useEffect, useState } from 'react';
import { api } from '../api/endpoints';
import type { MenuScreen, MessageColor } from '../api/types';
import { Field } from '../components/Field';
import { Screen, type MessageKind } from '../components/Screen';
import { useProgramNav } from '../lib/navigation';
import { useMessages } from '../lib/useMessages';
import { page } from '../programs';
import { useSession } from '../session/session';

const KIND: Record<MessageColor, MessageKind> = { DEFAULT: 'info', RED: 'error', GREEN: 'success' };

/** COMEN01 / COMEN1A — COMEN01C (CM00) and COADM01 / COADM1A — COADM01C (CA00). Options come from the API. */
export function MenuPage({ menu }: { menu: 'main' | 'admin' }) {
  const PAGE = page(menu === 'admin' ? 'COADM01C' : 'COMEN01C');
  const { go, nav } = useProgramNav();
  const { signOut } = useSession();
  const msg = useMessages();
  const [screen, setScreen] = useState<MenuScreen | null>(null);
  const [option, setOption] = useState('');
  const [busy, setBusy] = useState(false);
  const { fail, say } = msg;
  const handed = nav.context?.toProgram === PAGE.program ? nav.message : null;

  useEffect(() => {
    let live = true;
    api
      .menu(menu)
      .then((s) => {
        if (!live) return;
        setScreen(s);
        if (handed) say(handed, 'error');
      })
      .catch((err) => live && fail(err));
    return () => {
      live = false;
    };
  }, [menu, fail, say, handed]);

  const select = async (value: string = option) => {
    setBusy(true);
    try {
      const result = await api.menuSelect(menu, value);
      if (result.message?.trim()) {
        say(result.message, KIND[result.messageColor] ?? 'info');
        return;
      }
      if (!go(result.navigation)) {
        say(`${result.navigation.toProgram} has no web page yet`, 'error');
      }
    } catch (err) {
      fail(err);
    } finally {
      setBusy(false);
    }
  };

  const exit = async () => {
    try {
      const ctx = await api.menuExit(menu);
      const bye = await api.logout();
      go(ctx, { message: bye.message });
    } catch (err) {
      fail(err);
      signOut();
    }
  };

  const lines = Array.from({ length: 12 }, (_, i) => screen?.options[i] ?? null);

  return (
    <Screen
      page={PAGE}
      header={screen?.header}
      message={msg.message}
      busy={busy}
      pfKeys={[
        { key: 'ENTER', label: 'Continue', action: () => void select() },
        { key: 'F3', label: 'Exit', action: exit },
      ]}
    >
      <ol className="menu-options">
        {lines.map((opt, i) => (
          <li key={i} data-bms={`OPTN${String(i + 1).padStart(3, '0')}`}>
            {opt ? (
              <button
                type="button"
                className="menu-option"
                data-program={opt.programId}
                onClick={() => {
                  const value = String(opt.number);
                  setOption(value);
                  void select(value);
                }}
              >
                {opt.label}
              </button>
            ) : (
              screen?.optionLines[i] ?? ''
            )}
          </li>
        ))}
      </ol>
      <div className="form-grid narrow">
        <Field id="option" bms="OPTION" label="Please select an option" value={option} onChange={setOption} maxLength={2} numeric autoFocus invalid={msg.isInvalid('option')} />
      </div>
    </Screen>
  );
}
