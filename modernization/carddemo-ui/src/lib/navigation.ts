import { useCallback } from 'react';
import { useNavigate } from 'react-router-dom';
import type { NavigationContext } from '../api/types';
import { menuProgram, routeFor, type ProgramPage } from '../programs';
import { useSession, type NavState } from '../session/session';

export function transfer(from: ProgramPage, toProgram: string, base?: NavigationContext | null): NavigationContext {
  return {
    fromTranId: from.tranId,
    fromProgram: from.program,
    toTranId: null,
    toProgram,
    pgmContext: 'ENTER',
    custId: base?.custId ?? null,
    acctId: base?.acctId ?? null,
    cardNum: base?.cardNum ?? null,
  };
}

/** XCTL replacement: route by NavigationContext.toProgram and keep the context as the client COMMAREA. */
export function useProgramNav() {
  const navigate = useNavigate();
  const { session, nav, setNav, signOut } = useSession();

  const go = useCallback(
    (context: NavigationContext, extra: Omit<NavState, 'context'> = {}): boolean => {
      if (context.toProgram === 'COSGN00C') {
        signOut(extra.message ?? undefined);
        navigate('/signon');
        return true;
      }
      const route = routeFor(context.toProgram);
      if (!route) return false;
      setNav({ context, ...extra });
      navigate(route);
      return true;
    },
    [navigate, setNav, signOut],
  );

  /** PF3 without an API answer yet: back to CDEMO-FROM-PROGRAM, else the role's menu. */
  const back = useCallback(
    (from: ProgramPage, exit?: NavigationContext | null) => {
      if (exit?.toProgram) {
        // COMMAREA semantics: keys the exit does not set survive the transfer
        go({
          ...exit,
          acctId: exit.acctId ?? nav.context?.acctId ?? null,
          custId: exit.custId ?? nav.context?.custId ?? null,
          cardNum: exit.cardNum ?? nav.context?.cardNum ?? null,
        });
        return;
      }
      const caller = nav.context?.toProgram === from.program ? nav.context.fromProgram : null;
      const target = caller && caller !== from.program && routeFor(caller) ? caller : menuProgram(session?.role);
      go(transfer(from, target, nav.context));
    },
    [go, nav.context, session?.role],
  );

  /** The context handed to this page, if it was addressed to it. */
  const incoming = (program: string): NavState | null => (nav.context?.toProgram === program ? nav : null);

  return { go, back, incoming, nav };
}
