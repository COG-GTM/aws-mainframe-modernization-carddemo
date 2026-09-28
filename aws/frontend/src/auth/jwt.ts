import type { Role } from '../api/types';

export interface JwtClaims {
  sub: string;
  role: Role;
  name?: string;
  exp?: number;
}

function base64UrlDecode(segment: string): string {
  const base64 = segment.replace(/-/g, '+').replace(/_/g, '/');
  const padded = base64.padEnd(base64.length + ((4 - (base64.length % 4)) % 4), '=');
  const binary = atob(padded);
  const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));
  return new TextDecoder().decode(bytes);
}

export function decodeJwt(token: string): JwtClaims | null {
  const parts = token.split('.');
  if (parts.length !== 3) return null;
  try {
    const claims = JSON.parse(base64UrlDecode(parts[1])) as Partial<JwtClaims>;
    if (typeof claims.sub !== 'string' || (claims.role !== 'ADMIN' && claims.role !== 'USER')) return null;
    return claims as JwtClaims;
  } catch {
    return null;
  }
}

export function isExpired(claims: JwtClaims, now: number = Date.now()): boolean {
  return typeof claims.exp === 'number' && claims.exp * 1000 <= now;
}
