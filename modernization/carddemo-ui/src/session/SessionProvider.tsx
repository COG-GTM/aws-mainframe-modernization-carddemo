import { useCallback, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react';
import { configureClient } from '../api/client';
import type { LoginResponse } from '../api/types';
import { NAV_KEY, readNav, readSession, SessionContext, TOKEN_KEY, type NavState, type Session, type SessionState } from './session';

export function SessionProvider({ children }: { children: ReactNode }) {
  const [session, setSession] = useState<Session | null>(() => readSession());
  const [nav, setNavState] = useState<NavState>(() => readNav());
  const [signOffMessage, setSignOffMessage] = useState<string | null>(null);
  const tokenRef = useRef<string | null>(session?.token ?? null);

  const signOut = useCallback((message?: string) => {
    sessionStorage.removeItem(TOKEN_KEY);
    sessionStorage.removeItem(NAV_KEY);
    tokenRef.current = null;
    setSession(null);
    setNavState({ context: null });
    setSignOffMessage(message ?? null);
  }, []);

  const signIn = useCallback((login: LoginResponse) => {
    const next: Session = {
      token: login.token,
      expiresAt: login.expiresAt,
      userId: login.userId,
      role: login.role,
      userType: login.userType,
    };
    sessionStorage.setItem(TOKEN_KEY, JSON.stringify(next));
    tokenRef.current = next.token;
    setSession(next);
    setSignOffMessage(null);
  }, []);

  const setNav = useCallback((next: NavState) => {
    sessionStorage.setItem(NAV_KEY, JSON.stringify(next));
    setNavState(next);
  }, []);

  // layout effect: wired before the pages' passive effects issue their first request
  useLayoutEffect(() => {
    configureClient(
      () => tokenRef.current,
      (problem) => signOut(problem.message),
    );
  }, [signOut]);

  const value = useMemo<SessionState>(
    () => ({ session, nav, signIn, signOut, setNav, signOffMessage }),
    [session, nav, signIn, signOut, setNav, signOffMessage],
  );
  return <SessionContext.Provider value={value}>{children}</SessionContext.Provider>;
}
