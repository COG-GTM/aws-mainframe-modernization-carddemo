import type { ApiProblem } from './types';

export const API_PREFIX = '/api/v1';

/** A non-2xx answer from the API, carrying the RFC 7807 `code` / `field` / `message` (ADR-0019). */
export class ApiError extends Error {
  readonly status: number;
  readonly problem: ApiProblem;

  constructor(status: number, problem: ApiProblem) {
    super(problem.message);
    this.status = status;
    this.problem = problem;
  }

  get code(): string {
    return this.problem.code;
  }

  get field(): string | null {
    return this.problem.field ?? null;
  }
}

type Query = Record<string, string | number | boolean | null | undefined>;

let tokenProvider: () => string | null = () => null;
let unauthorizedHandler: (problem: ApiProblem) => void = () => {};

export function configureClient(token: () => string | null, onUnauthorized: (problem: ApiProblem) => void): void {
  tokenProvider = token;
  unauthorizedHandler = onUnauthorized;
}

export function buildUrl(path: string, query?: Query): string {
  const params = new URLSearchParams();
  for (const [key, value] of Object.entries(query ?? {})) {
    if (value !== undefined && value !== null && value !== '') params.set(key, String(value));
  }
  const qs = params.toString();
  return `${API_PREFIX}${path}${qs ? `?${qs}` : ''}`;
}

async function problemOf(response: Response): Promise<ApiProblem> {
  try {
    const body = (await response.json()) as Partial<ApiProblem>;
    if (body && typeof body.message === 'string') {
      return { code: body.code ?? 'ERROR', field: body.field ?? null, status: response.status, ...body } as ApiProblem;
    }
  } catch {
    // not a problem body
  }
  return { code: 'ERROR', field: null, status: response.status, message: `Unexpected response (HTTP ${response.status})` };
}

async function send(method: string, path: string, options: { body?: unknown; query?: Query; accept?: string }) {
  const headers: Record<string, string> = { Accept: options.accept ?? 'application/json' };
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
    throw new ApiError(0, { code: 'NETWORK', field: null, status: 0, message: 'Unable to reach the CardDemo API' });
  }
  if (!response.ok) {
    const problem = await problemOf(response);
    if (response.status === 401 && path !== '/auth/login') unauthorizedHandler(problem);
    throw new ApiError(response.status, problem);
  }
  return response;
}

export async function apiRequest<T>(method: string, path: string, options: { body?: unknown; query?: Query } = {}): Promise<T> {
  const response = await send(method, path, options);
  const text = await response.text();
  return (text ? JSON.parse(text) : undefined) as T;
}

export async function apiDownload(path: string): Promise<Blob> {
  const response = await send('GET', path, { accept: 'application/octet-stream' });
  return response.blob();
}

export function errorText(err: unknown): string {
  if (err instanceof ApiError) return err.message;
  return err instanceof Error ? err.message : String(err);
}
