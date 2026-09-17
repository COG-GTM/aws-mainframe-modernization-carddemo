/**
 * HTTP transport for the signon and menu programs.
 *
 * One route per CICS program; the body carries the AID key, the map fields and
 * the commarea, and the response is the handler result verbatim. The transport
 * only parses and validates, so every behaviour stays testable without HTTP.
 */

import express, { type Express, type Request, type Response } from "express";

import { adminMenu, type AdminMenuOptions } from "./coadm01c.js";
import { createCommarea, PgmContext, type CardDemoCommarea } from "./commarea.js";
import { mainMenu, type MenuOptions } from "./comen01c.js";
import { signon, type SignonInputFields, type SignonOptions } from "./cosgn00c.js";
import type { MenuInputFields } from "./menu-screen.js";
import { AidKey, isAidKey, type AidKeyCode, type HandlerRequest } from "./screen.js";
import { UserSecurityFile, type UserSecurityStore } from "./user-security.js";

export interface AppOptions {
  /** Defaults to the `USRSEC` dataset shipped in `app/data`. */
  readonly users?: UserSecurityStore;
  readonly now?: () => Date;
  readonly applid?: string;
  readonly sysid?: string;
  readonly isProgramInstalled?: (program: string) => boolean;
}

class BadRequestError extends Error {}

function asRecord(value: unknown): Record<string, unknown> {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new BadRequestError("body must be an object");
  }
  return value as Record<string, unknown>;
}

function asString(value: unknown, field: string): string {
  if (value === undefined) {
    return "";
  }
  if (typeof value !== "string") {
    throw new BadRequestError(`${field} must be a string`);
  }
  return value;
}

function parseAid(value: unknown): AidKeyCode {
  if (value === undefined) {
    return AidKey.enter;
  }
  const aid = asString(value, "aid").toUpperCase();
  if (!isAidKey(aid)) {
    throw new BadRequestError(`unknown aid key ${aid}`);
  }
  return aid;
}

function parseCommarea(value: unknown): CardDemoCommarea | undefined {
  if (value === undefined || value === null) {
    return undefined;
  }
  const raw = asRecord(value);
  const base = createCommarea();
  const parsed: Record<string, unknown> = { ...base };
  for (const [key, current] of Object.entries(base)) {
    const supplied = raw[key];
    if (supplied === undefined) {
      continue;
    }
    if (typeof supplied !== typeof current) {
      throw new BadRequestError(`commarea.${key} must be a ${typeof current}`);
    }
    parsed[key] = supplied;
  }
  const pgmContext = parsed["pgmContext"];
  if (pgmContext !== PgmContext.enter && pgmContext !== PgmContext.reenter) {
    throw new BadRequestError("commarea.pgmContext must be 0 or 1");
  }
  return parsed as unknown as CardDemoCommarea;
}

function parseRequest<TFields>(
  body: unknown,
  readFields: (fields: Record<string, unknown>) => TFields,
): HandlerRequest<TFields> {
  const raw = asRecord(body);
  const fields = raw["fields"] === undefined ? {} : asRecord(raw["fields"]);
  const commarea = parseCommarea(raw["commarea"]);
  const request = { aid: parseAid(raw["aid"]), fields: readFields(fields) };
  return commarea === undefined ? request : { ...request, commarea };
}

const readSignonFields = (fields: Record<string, unknown>): SignonInputFields => ({
  userid: asString(fields["userid"], "fields.userid"),
  passwd: asString(fields["passwd"], "fields.passwd"),
});

const readMenuFields = (fields: Record<string, unknown>): MenuInputFields => ({
  option: asString(fields["option"], "fields.option"),
});

function handle(response: Response, run: () => unknown): void {
  try {
    response.json(run());
  } catch (error) {
    if (error instanceof BadRequestError) {
      response.status(400).json({ error: error.message });
      return;
    }
    throw error;
  }
}

export function createApp(options: AppOptions = {}): Express {
  const users = options.users ?? UserSecurityFile.open();
  const clock = options.now ?? ((): Date => new Date());

  const signonOptions = (): SignonOptions => ({
    users,
    now: clock(),
    applid: options.applid ?? "",
    sysid: options.sysid ?? "",
  });
  const menuOptions = (): MenuOptions & AdminMenuOptions => ({
    now: clock(),
    isProgramInstalled: options.isProgramInstalled ?? ((): boolean => true),
  });

  const app = express();
  app.use(express.json());

  app.post("/signon", (request: Request, response: Response) => {
    handle(response, () => signon(parseRequest(request.body, readSignonFields), signonOptions()));
  });

  app.post("/menu", (request: Request, response: Response) => {
    handle(response, () => mainMenu(parseRequest(request.body, readMenuFields), menuOptions()));
  });

  app.post("/admin", (request: Request, response: Response) => {
    handle(response, () => adminMenu(parseRequest(request.body, readMenuFields), menuOptions()));
  });

  return app;
}
