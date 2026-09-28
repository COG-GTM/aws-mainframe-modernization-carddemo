import { useCallback, useLayoutEffect, useMemo, useRef, useState, type ReactNode } from 'react';
import { configureClient } from '../api/client';
import { AuthContext, type AuthState, type Session } from './context';
import { decodeJwt, isExpired } from './jwt';

const STORAGE_KEY = 'carddemo.token';

function sessionFromToken(token: string | null): Session | null {
  if (!token) return null;
  const claims = decodeJwt(token);
  if (!claims || isExpired(claims)) return null;
  return { token, userId: claims.sub, role: claims.role, name: claims.name ?? claims.sub };
}

export function AuthProvider({ children }: { children: ReactNode }) {
  const [session, setSession] = useState<Session | null>(() => sessionFromToken(sessionStorage.getItem(STORAGE_KEY)));
  const tokenRef = useRef<string | null>(session?.token ?? null);

  const signOut = useCallback(() => {
    sessionStorage.removeItem(STORAGE_KEY);
    tokenRef.current = null;
    setSession(null);
  }, []);

  const signIn = useCallback((token: string) => {
    const next = sessionFromToken(token);
    if (!next) throw new Error('Unable to verify the User ...');
    sessionStorage.setItem(STORAGE_KEY, token);
    tokenRef.current = token;
    setSession(next);
    return next;
  }, []);

  // layout effect: must be wired before the children's (passive) effects issue their first request
  useLayoutEffect(() => {
    configureClient(() => tokenRef.current, signOut);
  }, [signOut]);

  const value = useMemo<AuthState>(() => ({ session, signIn, signOut }), [session, signIn, signOut]);
  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}
