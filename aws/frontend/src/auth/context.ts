import { createContext, useContext } from 'react';
import type { Role } from '../api/types';

export interface Session {
  token: string;
  userId: string;
  role: Role;
  name: string;
}

export interface AuthState {
  session: Session | null;
  signIn: (token: string) => Session;
  signOut: () => void;
}

export const AuthContext = createContext<AuthState | null>(null);

export function useAuth(): AuthState {
  const ctx = useContext(AuthContext);
  if (!ctx) throw new Error('useAuth must be used inside <AuthProvider>');
  return ctx;
}

export function homeRoute(role: Role | undefined): string {
  return role === 'ADMIN' ? '/admin' : '/menu';
}
