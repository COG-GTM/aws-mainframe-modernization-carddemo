import { API_PREFIX } from '../config';
import type { ErrorEnvelope, FieldError } from './types';

export class ApiError extends Error {
  readonly status: number;
  readonly errorCode: string;
  readonly fieldErrors: FieldError[];
  readonly legacyProgram?: string;

  constructor(status: number, envelope: ErrorEnvelope) {
    super(envelope.message);
    this.name = 'ApiError';
    this.status = status;
    this.errorCode = envelope.errorCode;
    this.fieldErrors = envelope.fieldErrors ?? [];
    this.legacyProgram = envelope.legacyProgram;
  }
}

type TokenProvider = () => string | null;
type UnauthorizedHandler = () => void;

let tokenProvider: TokenProvider = () => null;
let unauthorizedHandler: UnauthorizedHandler = () => undefined;

export function configureClient(provider: TokenProvider, onUnauthorized: UnauthorizedHandler): void {
  tokenProvider = provider;
  unauthorizedHandler = onUnauthorized;
}

export type Query = Record<string, string | number | undefined | null>;

function buildUrl(path: string, query?: Query): string {
  const params = new URLSearchParams();
  for (const [key, value] of Object.entries(query ?? {})) {
    if (value !== undefined && value !== null && value !== '') params.set(key, String(value));
  }
  const qs = params.toString();
  const url = new URL(`${API_PREFIX}${path}`, window.location.origin);
  url.search = qs;
  return url.toString();
}

async function parseError(response: Response): Promise<ErrorEnvelope> {
  try {
    const body = (await response.json()) as Partial<ErrorEnvelope>;
    if (body && typeof body.message === 'string') {
      return { errorCode: body.errorCode ?? 'INTERNAL_ERROR', ...body } as ErrorEnvelope;
    }
  } catch {
    // fall through to the generic envelope
  }
  return { errorCode: 'INTERNAL_ERROR', message: `Unexpected response (HTTP ${response.status})` };
}

export async function apiRequest<T>(method: string, path: string, options: { body?: unknown; query?: Query } = {}): Promise<T> {
  const headers: Record<string, string> = { Accept: 'application/json' };
  const token = tokenProvider();
  if (token) headers.Authorization = `Bearer ${token}`;
  if (options.body !== undefined) headers['Content-Type'] = 'application/json';

  let response: Response;
  try {
    response = await fetch(buildUrl(path, options.query), {
      method,
      headers,
      body: options.body === undefined ? undefined : JSON.stringify(options.body),
    });
  } catch {
    throw new ApiError(0, { errorCode: 'NETWORK_ERROR', message: 'Unable to reach the CardDemo service...' });
  }

  if (!response.ok) {
    const envelope = await parseError(response);
    if (response.status === 401 && path !== '/auth/signon') unauthorizedHandler();
    throw new ApiError(response.status, envelope);
  }
  if (response.status === 204) return undefined as T;
  const text = await response.text();
  return (text ? JSON.parse(text) : undefined) as T;
}

export function errorMessage(error: unknown): string {
  if (error instanceof ApiError) return error.fieldErrors[0]?.message ?? error.message;
  if (error instanceof Error) return error.message;
  return 'Unexpected error';
}
