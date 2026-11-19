// Shared data, indexes and helpers for every LegacyLens view.
export const S = { ready: false };
export const GH = 'https://github.com/COG-GTM/aws-mainframe-modernization-carddemo';
export const TYPES = {
  job: { label: 'Batch job', plural: 'Batch jobs' }, transaction: { label: 'CICS transaction', plural: 'CICS transactions' },
  program: { label: 'Program', plural: 'Programs' }, screen: { label: 'Screen map', plural: 'Screen maps' },
  dataset: { label: 'Data store', plural: 'Data stores' }, copybook: { label: 'Copybook', plural: 'Copybooks' },
  external: { label: 'Platform service', plural: 'Platform services' },
};
export const KINDS = {
  triggers: 'triggers', runs: 'runs', starts: 'starts', calls: 'calls', transfers: 'transfers to', displays: 'displays',
  reads: 'is read by', writes: 'writes', includes: 'includes', defines: 'defines', generates: 'generates',
};
export const STAGES = [
  { id: 'discovered', label: 'Discovered', gate: 'Scan finding reviewed' },
  { id: 'characterized', label: 'Characterized', gate: 'SME approves the rules' },
  { id: 'converted', label: 'Converted', gate: 'Engineer reviews the PR' },
  { id: 'validated', label: 'Validated', gate: 'Parity evidence signed off' },
  { id: 'cutover', label: 'Cut over', gate: 'Go / no-go by the system owner' },
];
export const APPROACH = {
  refactor: 'Refactor', rewrite: 'Rewrite', replace: 'Replace (COTS/GOTS)', retire: 'Retire into platform',
};

export const esc = (s) => String(s ?? '').replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
export const h = (html) => { const t = document.createElement('template'); t.innerHTML = html.trim(); return t.content.firstElementChild; };
export const plural = (n, w) => `${n} ${w}${n === 1 ? '' : 's'}`;

export async function loadAll() {
  const get = (k) => fetch('/api/data/' + k).then((r) => r.json());
  const [estate, findings, rules, prs, compliance, tracker] = await Promise.all([
    get('estate'), get('findings'), get('rules'), get('prs'), get('compliance'), fetch('/api/tracker').then((r) => r.json())]);
  Object.assign(S, { estate, findings, rules, prs, compliance });
  S.N = new Map(estate.nodes.map((n) => [n.id, n]));
  S.out = new Map(); S.inn = new Map();
  for (const n of estate.nodes) { S.out.set(n.id, []); S.inn.set(n.id, []); }
  for (const e of estate.edges) { S.out.get(e.source).push(e); S.inn.get(e.target).push(e); }
  S.fById = new Map(findings.map((f) => [f.id, f]));
  S.fByNode = group(findings, (f) => f.nodes);
  S.fPrimary = group(findings, (f) => f.primary);
  S.prByNode = group(prs, (p) => p.nodes);
  S.rulesByProg = group(rules, (r) => ['pgm:' + r.program]);
  S.fileNode = new Map(estate.files.filter((f) => f.node).map((f) => [f.path, f.node]));
  S.tracker = new Map(tracker.map((t) => [t.node_id, t]));
  S.units = estate.nodes.filter((n) => n.type === 'program').sort((a, b) => a.label.localeCompare(b.label));
  S.ready = true;
}
function group(list, keys) {
  const m = new Map();
  for (const x of list) for (const k of keys(x)) { if (!m.has(k)) m.set(k, []); m.get(k).push(x); }
  return m;
}

// ---- migration stage: derived from evidence, overridable in the tracker ----
export function evidenceStage(n) {
  const ev = [[], [], [], [], []];
  const fs = S.fByNode.get(n.id) || [];
  if (fs.length) ev[0].push({ t: `${plural(fs.length, 'scan finding')} document this component`, passed: true });
  const rules = S.rulesByProg.get(n.id) || [];
  if (rules.length) ev[1].push({ t: `${rules.length} business rules catalogued on branch ${rules[0].branch}, awaiting SME approval`, passed: false });
  for (const p of S.prByNode.get(n.id) || []) {
    if (p.state === 'CLOSED') continue;  // closed without merging: not evidence
    const t = (p.about && p.about[n.id]) || p.title;
    const tag = { t: `PR #${p.number} (${p.state === 'MERGED' ? 'merged into ' + p.base : 'open'})`, passed: p.state === 'MERGED' };
    if (/sfind/i.test(t + ' ' + p.branch)) ev[0].push({ ...tag, t: tag.t + ' fixes a finding in the COBOL' });
    else if (/parity|golden|gnucobol|equivalen|e2e|sign-off|audit/i.test(t)) ev[3].push(tag);
    else if (/^docs|requisitos|rules|inventory|diagram/i.test(t)) ev[1].push(tag);
    else if (/sfind|^bug|fix bug|\bmask\b|\brequire/i.test(t + ' ' + p.branch)) ev[0].push({ ...tag, t: tag.t + ' fixes a finding in the COBOL' });
    else ev[2].push(tag);
  }
  // a stage counts only when a PR for it was reviewed and merged (into a migration branch; none is on main); open work is "in review"
  let stage = 0, review = 0;
  for (let i = 0; i < 5; i++) { if (ev[i].some((x) => x.passed)) stage = i; if (ev[i].length) review = i; }
  return { stage, review, ev };
}
export function unitState(n) {
  const d = evidenceStage(n);
  const t = S.tracker.get(n.id);
  const manual = t && t.stage ? STAGES.findIndex((s) => s.id === t.stage) : -1;
  return { ...d, derived: d.stage, stage: manual >= 0 ? manual : d.stage, manual: manual >= 0, owner: t?.owner || '', note: t?.note || '', updated: t?.updated_at };
}
export async function saveTracker(id, patch) {
  const r = await fetch('/api/tracker/' + encodeURIComponent(id), { method: 'PUT', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(patch) });
  if (!r.ok) throw new Error('save failed');
  S.tracker.set(id, await r.json());
}
export async function resetTracker(id) {
  await fetch('/api/tracker/' + encodeURIComponent(id), { method: 'DELETE' });
  S.tracker.delete(id);
}

export function nextPrompt(n) {
  const st = unitState(n);
  const fs = (S.fPrimary.get(n.id) || S.fByNode.get(n.id) || []).slice(0, 3).map((f) => f.id);
  const cps = S.out.get(n.id).filter((e) => e.kind === 'includes').map((e) => S.N.get(e.target).label);
  const ds = [...new Set([...S.out.get(n.id).filter((e) => e.kind === 'writes').map((e) => S.N.get(e.target).label), ...S.inn.get(n.id).filter((e) => e.kind === 'reads').map((e) => S.N.get(e.source).label)])];
  const jobs = S.inn.get(n.id).filter((e) => e.kind === 'runs').map((e) => S.N.get(e.source).label);
  const head = `Repo: COG-GTM/aws-mainframe-modernization-carddemo\nComponent: ${n.label} (${n.file})` +
    (jobs.length ? `\nRun by: ${jobs.join(', ')}` : '') + (cps.length ? `\nCopybooks: ${cps.join(', ')}` : '') + (ds.length ? `\nData stores: ${ds.join(', ')}` : '') +
    (fs.length ? `\nScan findings: ${fs.join(', ')} (scan ${S.estate.meta.scan})` : '');
  const ap = n.approach;
  const steps = [
    `Characterize ${n.label}. Write its business-rules catalog from the source: one row per rule with class, inputs, outputs and file:line citations. Mark anything ambiguous as an SME question instead of guessing. Open a PR with the catalog only; do not convert code yet.`,
    `Convert ${n.label} following the approved rules catalog. Recommended route: ${APPROACH[ap.approach]} - ${ap.target}. Keep the COBOL behaviour exactly, including quirks the SME marked "preserve". Build a golden master first by running the original under GnuCOBOL on the sample data, then implement and open one PR for this component.`,
    `Validate the converted ${n.label} against the COBOL golden master. Compare every output record byte for byte, classify each difference as match, intended change or unresolved, and attach the evidence report to the PR. Do not merge.`,
    `Prepare cut-over for ${n.label}: write the parallel-run plan, the reconciliation query for its data stores, the rollback steps, and the go/no-go checklist for the system owner.`,
    `${n.label} is cut over. Propose the retirement PR: remove the COBOL source, its JCL and scheduler entries, and list any copybooks or datasets that no longer have a consumer.`,
  ];
  return `${head}\n\nCurrent stage: ${STAGES[st.stage].label}\nNext: ${steps[st.stage]}\n\nHuman gate before the following stage: ${STAGES[Math.min(st.stage + 1, 4)].gate}.`;
}

// ---- routing ----
export function route() {
  const [p, q] = location.hash.replace(/^#\/?/, '').split('?');
  return { view: p || 'map', params: Object.fromEntries(new URLSearchParams(q || '')) };
}
export function href(view, params = {}) {
  const q = new URLSearchParams(Object.entries(params).filter(([, v]) => v != null && v !== '')).toString();
  return '#/' + view + (q ? '?' + q : '');
}
export const go = (view, params) => { location.hash = href(view, params); };
export const srcHref = (file, line) => href('source', { f: file, l: line });
export function nodeHref(id) { return href('map', { n: id }); }
export function nodeChip(id) {
  const n = S.N.get(id); if (!n) return '';
  return `<a class="nodelink mono" href="${nodeHref(id)}" title="${esc(TYPES[n.type].label)}">${esc(n.label)}</a>`;
}

let toastT;
export function toast(msg) {
  const t = document.getElementById('toast'); t.textContent = msg; t.classList.add('show');
  clearTimeout(toastT); toastT = setTimeout(() => t.classList.remove('show'), 1800);
}
export async function copy(text, msg = 'Copied') {
  try { await navigator.clipboard.writeText(text); } catch { const a = document.createElement('textarea'); a.value = text; document.body.append(a); a.select(); document.execCommand('copy'); a.remove(); }
  toast(msg);
}

// ---- markdown with source links and lazily loaded Mermaid ----
marked.use({ renderer: { html: (t) => esc(typeof t === 'string' ? t : t.text) } });
let mermaidP;
function loadMermaid() {
  return mermaidP ||= new Promise((res, rej) => {
    const s = document.createElement('script'); s.src = '/vendor/mermaid.min.js';
    s.onload = () => { window.mermaid.initialize({ startOnLoad: false, theme: 'base', securityLevel: 'strict', fontFamily: 'Geist, sans-serif',
      themeVariables: { primaryColor: '#FFFFFF', primaryBorderColor: '#141414', primaryTextColor: '#141414', lineColor: '#2600FF', fontSize: '13px', background: '#F7F6F5' } }); res(window.mermaid); };
    s.onerror = rej; document.head.append(s);
  });
}
let mid = 0;
export function md(text) {
  const el = document.createElement('div'); el.className = 'md';
  el.innerHTML = marked.parse(text || '');
  // imported documents may only link to http(s) pages
  el.querySelectorAll('a[href]').forEach((a) => { if (!/^https?:\/\//i.test(a.getAttribute('href'))) a.replaceWith(...a.childNodes); else { a.target = '_blank'; a.rel = 'noopener'; } });
  const files = new Set(S.estate.files.map((f) => f.path));
  el.querySelectorAll('code').forEach((c) => {
    if (c.parentElement.tagName === 'PRE') return;
    const m = c.textContent.match(/^((?:app|scripts)\/[\w./-]+?)(?::(\d+))?(?:-\d+)?$/);
    if (m && files.has(m[1])) { const a = document.createElement('a'); a.href = srcHref(m[1], m[2]); a.append(c.cloneNode(true)); c.replaceWith(a); return; }
    const id = ['pgm:', 'job:', 'cpy:', 'map:', 'txn:'].map((p) => p + c.textContent).find((k) => S.N.has(k));
    if (id) { const a = document.createElement('a'); a.href = nodeHref(id); a.title = 'Show in estate map'; a.append(c.cloneNode(true)); c.replaceWith(a); }
  });
  el.querySelectorAll('pre > code.language-mermaid').forEach((c) => {
    const box = document.createElement('div'); box.className = 'mermaid-box'; box.textContent = 'Rendering flow…';
    const src = c.textContent; c.parentElement.replaceWith(box);
    loadMermaid().then((m) => m.render('mmd' + (++mid), src)).then((r) => { box.innerHTML = r.svg; })
      .catch(() => { box.innerHTML = '<pre></pre>'; box.firstChild.textContent = src; });
  });
  return el;
}
export function sevTag(s) { return `<span class="tag ${esc(s)}">${esc(s)}</span>`; }
