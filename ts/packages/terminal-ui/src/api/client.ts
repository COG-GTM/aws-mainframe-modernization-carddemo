/**
 * The single call the terminal makes: one screen exchange, the browser side of
 * the CICS pseudo conversation. The request carries what the operator typed
 * plus the AID key; the response carries the next screen, its fields, the
 * message line and the COMMAREA style state to echo back next time.
 */

import type { AidKey } from "../terminal/aid.js";
import type { ScreenFields } from "../terminal/screen-model.js";

/** The COMMAREA the program hands back; opaque to the UI, echoed unchanged. */
export type ScreenState = Readonly<Record<string, unknown>>;

export interface ScreenExchangeRequest {
  /** The program that owns the screen being submitted, e.g. `COSGN00C`. */
  readonly program: string;
  readonly screenFields: ScreenFields;
  readonly aidKey: AidKey;
  readonly state: ScreenState | null;
}

export interface ScreenExchangeResponse {
  /** The program that owns the screen to display next. */
  readonly program: string;
  readonly screenFields: ScreenFields;
  /** The error or information line the program set. */
  readonly message: string;
  readonly state: ScreenState | null;
}

export interface ScreenClient {
  exchange(request: ScreenExchangeRequest): Promise<ScreenExchangeResponse>;
}

export class ScreenExchangeError extends Error {}

function isScreenFields(value: unknown): value is ScreenFields {
  return (
    typeof value === "object" &&
    value !== null &&
    Object.values(value).every((entry) => typeof entry === "string")
  );
}

/** Narrows the JSON body of the Express endpoint to the response contract. */
export function parseScreenExchangeResponse(body: unknown): ScreenExchangeResponse {
  if (typeof body !== "object" || body === null) {
    throw new ScreenExchangeError("the screen exchange response is not an object");
  }

  const { program, screenFields, message, state } = body as Record<string, unknown>;
  if (typeof program !== "string" || !isScreenFields(screenFields) || typeof message !== "string") {
    throw new ScreenExchangeError("the screen exchange response is missing program, screenFields or message");
  }

  return {
    program,
    screenFields,
    message,
    state: typeof state === "object" && state !== null ? (state as ScreenState) : null,
  };
}

/** Talks to the Express API that hosts the migrated online programs. */
export function createHttpScreenClient(baseUrl = "/api"): ScreenClient {
  return {
    async exchange(request: ScreenExchangeRequest): Promise<ScreenExchangeResponse> {
      const response = await fetch(`${baseUrl}/screen`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(request),
      });

      if (!response.ok) {
        throw new ScreenExchangeError(
          `the screen exchange failed with HTTP ${String(response.status)}`,
        );
      }

      return parseScreenExchangeResponse(await response.json());
    },
  };
}
