// LegacyLens server: static UI, read-only estate data, source files from the repo checkout,
// and a small SQLite store for tracker edits (kept outside the app directory).
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import zlib from 'node:zlib';
import { fileURLToPath } from 'node:url';
import { DatabaseSync } from 'node:sqlite';

const HERE = path.dirname(fileURLToPath(import.meta.url));
const REPO = path.resolve(HERE, '..');
const PORT = Number(process.env.PORT || 4310);
const DATA_DIR = process.env.LEGACYLENS_DATA || path.join(os.homedir(), 'legacylens-data');
fs.mkdirSync(DATA_DIR, { recursive: true });

const load = (f) => fs.readFileSync(path.join(HERE, 'data', f));
const data = { estate: load('estate.json'), findings: load('findings.json'), rules: load('rules.json'), prs: load('prs.json'), compliance: load('compliance.json') };
const gz = Object.fromEntries(Object.entries(data).map(([k, v]) => [k, zlib.gzipSync(v)]));
const estate = JSON.parse(data.estate);
const fileIndex = new Map(estate.files.map((f) => [f.path, f]));
const nodeIds = new Set(estate.nodes.map((n) => n.id));

const db = new DatabaseSync(path.join(DATA_DIR, 'tracker.db'));
db.exec(`CREATE TABLE IF NOT EXISTS tracker (
  node_id TEXT PRIMARY KEY, stage TEXT, owner TEXT, note TEXT, updated_at TEXT NOT NULL)`);
const STAGES = ['discovered', 'characterized', 'converted', 'validated', 'cutover'];

const TYPES = { '.html': 'text/html; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.css': 'text/css; charset=utf-8',
  '.svg': 'image/svg+xml', '.ttf': 'font/ttf', '.json': 'application/json', '.png': 'image/png', '.ico': 'image/x-icon' };
const VENDOR = {
  '/vendor/d3.min.js': 'node_modules/d3/dist/d3.min.js',
  '/vendor/marked.min.js': 'node_modules/marked/marked.min.js',
  '/vendor/mermaid.min.js': 'node_modules/mermaid/dist/mermaid.min.js',
};

function send(res, code, body, type = 'application/json', extra = {}) {
  res.writeHead(code, { 'Content-Type': type, 'Cache-Control': 'no-cache', ...extra });
  res.end(body);
}
const json = (res, code, obj) => send(res, code, JSON.stringify(obj));

function hexdump(buf, max = 4096) {
  const out = [];
  for (let o = 0; o < Math.min(buf.length, max); o += 16) {
    const row = buf.subarray(o, o + 16);
    const hex = [...row].map((b) => b.toString(16).padStart(2, '0')).join(' ');
    const asc = [...row].map((b) => (b >= 32 && b < 127 ? String.fromCharCode(b) : '.')).join('');
    out.push(o.toString(16).padStart(8, '0') + '  ' + hex.padEnd(48) + ' |' + asc + '|');
  }
  return out.join('\n');
}

function readBody(req) {
  return new Promise((resolve, reject) => {
    let s = '';
    req.on('data', (c) => { s += c; if (s.length > 1e5) { reject(new Error('too large')); req.destroy(); } });
    req.on('end', () => { try { resolve(JSON.parse(s || '{}')); } catch (e) { reject(e); } });
  });
}

const server = http.createServer(async (req, res) => {
  try {
    const url = new URL(req.url, 'http://x');
    const p = url.pathname;
    if (p.startsWith('/api/data/')) {
      const k = p.slice(10);
      if (!Object.hasOwn(gz, k)) return json(res, 404, { error: 'unknown dataset' });
      if (/gzip/.test(req.headers['accept-encoding'] || '')) return send(res, 200, gz[k], 'application/json', { 'Content-Encoding': 'gzip' });
      return send(res, 200, data[k]);
    }
    if (p === '/api/file') {
      const f = fileIndex.get(url.searchParams.get('path') || '');
      if (!f) return json(res, 404, { error: 'not a tracked source file' });
      const buf = fs.readFileSync(path.join(REPO, f.path));
      if (f.binary) return json(res, 200, { ...f, text: hexdump(buf), truncated: buf.length > 4096 });
      let text = buf.toString('utf8');
      if (text.includes('\uFFFD')) text = buf.toString('latin1');
      return json(res, 200, { ...f, text });
    }
    if (p === '/api/tracker' && req.method === 'GET') {
      return json(res, 200, db.prepare('SELECT * FROM tracker').all());
    }
    const m = p.match(/^\/api\/tracker\/(.+)$/);
    if (m && (req.method === 'PUT' || req.method === 'DELETE')) {
      const id = decodeURIComponent(m[1]);
      if (!nodeIds.has(id)) return json(res, 404, { error: 'unknown component' });
      if (req.method === 'DELETE') { db.prepare('DELETE FROM tracker WHERE node_id = ?').run(id); return json(res, 200, { ok: true }); }
      const b = await readBody(req);
      if (b.stage != null && !STAGES.includes(b.stage)) return json(res, 400, { error: 'invalid stage' });
      // only the fields present in the request change, so two people editing different fields do not undo each other
      const cur = db.prepare('SELECT * FROM tracker WHERE node_id = ?').get(id) || { stage: null, owner: '', note: '' };
      const row = { node_id: id, stage: 'stage' in b ? b.stage : cur.stage, owner: 'owner' in b ? String(b.owner ?? '').slice(0, 80) : cur.owner,
        note: 'note' in b ? String(b.note ?? '').slice(0, 2000) : cur.note, updated_at: new Date().toISOString() };
      db.prepare(`INSERT INTO tracker (node_id, stage, owner, note, updated_at) VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(node_id) DO UPDATE SET stage = excluded.stage, owner = excluded.owner, note = excluded.note, updated_at = excluded.updated_at`)
        .run(row.node_id, row.stage, row.owner, row.note, row.updated_at);
      return json(res, 200, row);
    }
    if (p.startsWith('/api/')) return json(res, 404, { error: 'not found' });
    if (VENDOR[p]) return send(res, 200, fs.readFileSync(path.join(HERE, VENDOR[p])), TYPES['.js'], { 'Cache-Control': 'public, max-age=86400' });
    let rel = path.normalize(p === '/' ? '/index.html' : p);
    let file = path.join(HERE, 'public', rel);
    if (!file.startsWith(path.join(HERE, 'public')) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) file = path.join(HERE, 'public', 'index.html');
    return send(res, 200, fs.readFileSync(file), TYPES[path.extname(file)] || 'application/octet-stream');
  } catch (e) {
    console.error(e);
    json(res, 500, { error: 'server error' });
  }
});
server.listen(PORT, '0.0.0.0', () => console.log(`LegacyLens listening on :${PORT}`));
