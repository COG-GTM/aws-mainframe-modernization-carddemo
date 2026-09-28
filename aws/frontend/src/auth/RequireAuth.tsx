import type { ReactNode } from 'react';
import { Navigate, useLocation } from 'react-router-dom';
import type { Role } from '../api/types';
import { useAuth } from './context';

export const ADMIN_ONLY_MESSAGE = 'No access - Admin Only option... ';

export function RequireAuth({ role, children }: { role?: Role; children: ReactNode }) {
  const { session } = useAuth();
  const location = useLocation();
  if (!session) return <Navigate to="/login" replace state={{ from: location.pathname }} />;
  if (role === 'ADMIN' && session.role !== 'ADMIN') {
    return <Navigate to="/menu" replace state={{ message: ADMIN_ONLY_MESSAGE }} />;
  }
  return <>{children}</>;
}
