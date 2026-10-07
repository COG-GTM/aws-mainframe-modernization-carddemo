import { render } from '@testing-library/react';
import { MemoryRouter, useLocation } from 'react-router-dom';
import { vi } from 'vitest';
import { App } from '../App';
import type { NavigationContext, Role } from '../api/types';
import { SessionProvider } from '../session/SessionProvider';
import { NAV_KEY, TOKEN_KEY, type NavState } from '../session/session';

export interface Call {
  method: string;
  path: string;
  query: URLSearchParams;
  body: unknown;
  auth: string | null;
}

type Reply = { status?: number; body?: unknown };
type Handler = (call: Call) => Reply | undefined;

/** A fetch double keyed on "METHOD /path" (path relative to /api/v1); records every call. */
export function mockApi(routes: Record<string, Reply | Handler>) {
  const calls: Call[] = [];
  const fetchMock = vi.fn(async (input: RequestInfo | URL, init?: RequestInit) => {
    const url = new URL(String(input), 'http://localhost');
    const method = (init?.method ?? 'GET').toUpperCase();
    const path = url.pathname.replace(/^\/api\/v1/, '');
    const headers = new Headers(init?.headers);
    const call: Call = {
      method,
      path,
      query: url.searchParams,
      body: init?.body ? JSON.parse(String(init.body)) : undefined,
      auth: headers.get('Authorization'),
    };
    calls.push(call);
    const route = routes[`${method} ${path}`];
    const reply = typeof route === 'function' ? route(call) : route;
    if (!reply) return new Response(JSON.stringify({ code: 'NOTFND', field: null, message: `no mock for ${method} ${path}`, status: 404 }), { status: 404 });
    return new Response(reply.body === undefined ? '' : JSON.stringify(reply.body), {
      status: reply.status ?? 200,
      headers: { 'Content-Type': 'application/json' },
    });
  });
  vi.stubGlobal('fetch', fetchMock);
  return { calls, fetchMock };
}

export function problem(status: number, code: string, message: string, field: string | null = null) {
  return { status, body: { type: 'about:blank', title: code, status, code, field, message } };
}

export function signedIn(role: Role, nav?: NavState) {
  sessionStorage.setItem(
    TOKEN_KEY,
    JSON.stringify({
      token: `test-token-${role}`,
      expiresAt: new Date(Date.now() + 3_600_000).toISOString(),
      userId: role === 'ADMIN' ? 'ADMIN001' : 'USER0001',
      role,
      userType: role === 'ADMIN' ? 'A' : 'U',
    }),
  );
  if (nav) sessionStorage.setItem(NAV_KEY, JSON.stringify(nav));
}

export function ctx(fromProgram: string, toProgram: string, extra: Partial<NavigationContext> = {}): NavigationContext {
  return {
    fromTranId: null,
    fromProgram,
    toTranId: null,
    toProgram,
    pgmContext: 'ENTER',
    custId: null,
    acctId: null,
    cardNum: null,
    ...extra,
  };
}

function Where() {
  const loc = useLocation();
  return <output data-testid="location">{loc.pathname}</output>;
}

export function renderAt(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <SessionProvider>
        <App />
        <Where />
      </SessionProvider>
    </MemoryRouter>,
  );
}

export const HEADER = {
  title01: 'AWS Mainframe Modernization',
  title02: 'CardDemo',
  tranId: 'XXXX',
  programName: 'XXXXXXXX',
  currentDate: '10/07/26',
  currentTime: '12:00:00',
  applId: 'CARDDEMO',
  sysId: 'CDMO',
};
