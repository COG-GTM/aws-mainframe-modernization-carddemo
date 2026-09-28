import { useCallback } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { homeRoute, useAuth } from '../auth/context';

export interface NavState {
  from?: string;
  message?: string;
}

/** PF3 semantics: return to the calling screen (legacy CDEMO-FROM-PROGRAM), else the role's menu. */
export function useBack(fallback?: string): () => void {
  const navigate = useNavigate();
  const location = useLocation();
  const { session } = useAuth();
  const state = (location.state ?? {}) as NavState;
  const target = state.from ?? fallback ?? homeRoute(session?.role);
  return useCallback(() => navigate(target), [navigate, target]);
}

export function useNavState(): NavState {
  const location = useLocation();
  return (location.state ?? {}) as NavState;
}
