import { createServer, type IncomingMessage, type Server, type ServerResponse } from 'node:http';
import { TerminalInputError, type OnlineRuntime } from '../framework/index.js';
import { createCardDemoRuntime } from '../programs.js';
import { TERMINAL_HTML } from './terminal-html.js';

interface Body {
  terminalId?: string;
  aid?: string;
  fields?: Record<string, string>;
}

async function readBody(req: IncomingMessage): Promise<Body> {
  const chunks: Buffer[] = [];
  for await (const c of req) chunks.push(c as Buffer);
  const text = Buffer.concat(chunks).toString('utf8').trim();
  if (!text) return {};
  const parsed: unknown = JSON.parse(text);
  if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) throw new TerminalInputError('body must be a JSON object');
  return parsed as Body;
}

function json(res: ServerResponse, status: number, body: unknown): void {
  res.writeHead(status, { 'content-type': 'application/json' });
  res.end(JSON.stringify(body));
}

/**
 * HTTP JSON API over the online runtime:
 * - `POST /api/transaction/{transid}` `{ terminalId?, aid?, fields? }` starts a transaction (new terminal if none given)
 * - `POST /api/terminal/{id}` `{ aid, fields }` sends input to the pending RETURN TRANSID
 * - `GET /api/terminal/{id}` current screen; `GET /api/transactions` transaction table; `GET /` browser terminal
 */
export function createOnlineServer(runtime: OnlineRuntime = createCardDemoRuntime()): Server {
  return createServer((req, res) => {
    void (async (): Promise<void> => {
      const url = new URL(req.url ?? '/', 'http://localhost');
      const parts = url.pathname.split('/').filter(Boolean).map(decodeURIComponent);
      try {
        if (req.method === 'GET' && parts.length === 0) {
          res.writeHead(200, { 'content-type': 'text/html; charset=utf-8' });
          res.end(TERMINAL_HTML);
        } else if (req.method === 'GET' && parts[0] === 'api' && parts[1] === 'transactions' && parts.length === 2) {
          json(res, 200, runtime.registry.transactionTable());
        } else if (req.method === 'POST' && parts[0] === 'api' && parts[1] === 'transaction' && parts.length === 3) {
          const body = await readBody(req);
          const term = runtime.openTerminal(body.terminalId);
          json(res, 200, runtime.startTransaction(term.id, parts[2]!, { aid: body.aid ?? 'ENTER', fields: body.fields ?? {} }));
        } else if (parts[0] === 'api' && parts[1] === 'terminal' && parts.length === 3) {
          if (req.method === 'GET') json(res, 200, runtime.snapshot(parts[2]!));
          else if (req.method === 'POST') {
            const body = await readBody(req);
            json(res, 200, runtime.sendInput(parts[2]!, { aid: body.aid ?? 'ENTER', fields: body.fields ?? {} }));
          } else json(res, 405, { error: 'method not allowed' });
        } else {
          json(res, 404, { error: 'not found' });
        }
      } catch (e) {
        if (e instanceof TerminalInputError || e instanceof SyntaxError) json(res, 400, { error: e.message });
        else if (e instanceof Error && e.message.startsWith('unknown AID')) json(res, 400, { error: e.message });
        else json(res, 500, { error: e instanceof Error ? e.message : String(e) });
      }
    })();
  });
}
