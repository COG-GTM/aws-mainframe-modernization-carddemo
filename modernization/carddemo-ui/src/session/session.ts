import { createContext, useContext } from 'react';
import type { LoginResponse, NavigationContext, Role } from '../api/types';

/** What survives a reload (sessionStorage, ADR-0017/0022): the JWT, the signed-on user and the COMMAREA replacement. */
export interface Session {
  token: string;
  expiresAt: string;
  userId: string;
  role: Role;
  userType: string;
}

/**
 * Client-side COMMAREA: the API's NavigationContext plus the keys one screen hands to the next
 * (opaque cardRef for cards, tranId, userId) — the API does not keep conversation state.
 */
export interface NavState {
  context: NavigationContext | null;
  cardRef?: string | null;
  tranId?: string | null;
  userId?: string | null;
  message?: string | null;
}

export interface SessionState {
  session: Session | null;
  nav: NavState;
  signIn: (login: LoginResponse) => void;
  signOut: (message?: string) => void;
  setNav: (nav: NavState) => void;
  signOffMessage: string | null;
}

export const SessionContext = createContext<SessionState | null>(null);

export function useSession(): SessionState {
  const ctx = useContext(SessionContext);
  if (!ctx) throw new Error('useSession must be used inside <SessionProvider>');
  return ctx;
}

export const TOKEN_KEY = 'carddemo.session';
export const NAV_KEY = 'carddemo.nav';

export function readSession(now: number = Date.now()): Session | null {
  try {
    const raw = sessionStorage.getItem(TOKEN_KEY);
    if (!raw) return null;
    const session = JSON.parse(raw) as Session;
    if (!session.token || Date.parse(session.expiresAt) <= now) return null;
    return session;
  } catch {
    return null;
  }
}

export function readNav(): NavState {
  try {
    const raw = sessionStorage.getItem(NAV_KEY);
    return raw ? (JSON.parse(raw) as NavState) : { context: null };
  } catch {
    return { context: null };
  }
}
