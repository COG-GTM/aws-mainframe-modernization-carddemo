// Source browser, per-program decomposition and the scan findings explorer.
import { S, TYPES, KINDS, STAGES, APPROACH, GH, esc, h, go, href, srcHref, nodeHref, nodeChip, md, sevTag, unitState, nextPrompt, copy, plural } from './core.js';

// ---------------------------------------------------------------- source ----
const KW = /\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE|DIVISION|SECTION|WORKING-STORAGE|LINKAGE|FILE-CONTROL|PERFORM|UNTIL|VARYING|IF|ELSE|END-IF|EVALUATE|WHEN|END-EVALUATE|MOVE|TO|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|CALL|USING|COPY|EXEC|END-EXEC|CICS|SQL|OPEN|CLOSE|READ|WRITE|REWRITE|DELETE|START|SELECT|ASSIGN|FD|PIC|PICTURE|VALUE|REDEFINES|OCCURS|COMP-3|COMP|BINARY|GOBACK|STOP|RUN|DISPLAY|ACCEPT|INITIALIZE|SET|STRING|UNSTRING|INSPECT|GO|EXIT|NOT|AND|OR|THRU|RETURN|XCTL|LINK|SEND|RECEIVE|MAP|MAPSET|DFHMSD|DFHMDI|DFHMDF|PGM|DD|DSN|DISP|JOB|PROC|PEND|SYSIN|DEFINE|CLUSTER|REPRO)\b/g;
function hl(line, lang) {
  const e = esc(line);
  if (lang === 'COBOL' || lang === 'Copybook') {
    if (line.length > 6 && (line[6] === '*' || line[6] === '/')) return `<span class="c">${e}</span>`;
  } else if ((lang === 'JCL' || lang === 'JCL proc') && line.startsWith('//*')) return `<span class="c">${e}</span>`;
  else if ((lang === 'BMS map' || lang === 'Assembler' || lang === 'CICS CSD') && line.startsWith('*')) return `<span class="c">${e}</span>`;
  else if (!['COBOL', 'Copybook', 'JCL', 'JCL proc', 'BMS map', 'Assembler', 'CICS CSD', 'Control card', 'DB2 DDL', 'DB2 DCLGEN'].includes(lang)) return e;
  return e.replace(/(&#39;[^&]*?&#39;)|\b([A-Z][A-Z0-9-]*)\b/g, (m, s, w) => {
    if (s) return `<span class="s">${s}</span>`;
    KW.lastIndex = 0;
    return KW.test(w) && RegExp.lastMatch === w ? `<span class="k">${w}</span>` : w;
  });
}
function buildTree(files) {
  const rootDir = { dirs: new Map(), files: [] };
  for (const f of files) {
    const parts = f.path.split('/'); let d = rootDir;
    for (const p of parts.slice(0, -1)) { if (!d.dirs.has(p)) d.dirs.set(p, { dirs: new Map(), files: [] }); d = d.dirs.get(p); }
    d.files.push(f);
  }
  return rootDir;
}
function treeHtml(d, path, open) {
  let s = '';
  for (const [name, sub] of [...d.dirs].sort((a, b) => a[0].localeCompare(b[0]))) {
    const p = path ? path + '/' + name : name;
    s += `<details ${open(p) ? 'open' : ''}><summary>${esc(name)}</summary><div>${treeHtml(sub, p, open)}</div></details>`;
  }
  for (const f of d.files.sort((a, b) => a.path.localeCompare(b.path))) s += `<a href="${srcHref(f.path)}" data-f="${esc(f.path)}">${esc(f.path.split('/').pop())}<small>${f.binary ? 'bin' : f.lines}</small></a>`;
  return s;
}

export function sourceView(root, params) {
  const files = S.estate.files;
  const el = h(`<div class="source"><nav class="tree" aria-label="Repository files"><input type="search" placeholder="Filter ${files.length} files…" aria-label="Filter files"><div class="t"></div></nav>
    <section class="codewrap"></section><aside class="refs"></aside></div>`);
  root.append(el);
  const tree = el.querySelector('.t'), wrap = el.querySelector('.codewrap'), refs = el.querySelector('.refs'), filter = el.querySelector('input');
  let cur = null;
  function drawTree() {
    const v = filter.value.trim().toLowerCase();
    const list = v ? files.filter((f) => f.path.toLowerCase().includes(v)) : files;
    const sel = cur || '';
    tree.innerHTML = treeHtml(buildTree(list), '', (p) => !!v || p === 'app' || sel.startsWith(p + '/')) || '<p class="muted" style="padding:6px">No files match.</p>';
    tree.querySelector(`a[data-f="${CSS.escape(sel)}"]`)?.classList.add('on');
  }
  filter.addEventListener('input', drawTree);
  async function show(p) {
    const path = p.f && files.some((f) => f.path === p.f) ? p.f : 'app/cbl/CBACT04C.cbl';
    const line = +p.l || 0;
    if (path !== cur) {
      cur = path; drawTree();
      tree.querySelector('a.on')?.scrollIntoView({ block: 'nearest' });
      wrap.innerHTML = '<p class="loading">Loading…</p>';
      const f = await fetch('/api/file?path=' + encodeURIComponent(path)).then((r) => r.json());
      if (cur !== path) return;
      const nid = S.fileNode.get(path), n = nid && S.N.get(nid);
      // lines that carry a parsed connection
      const marks = new Map();
      for (const e of S.estate.edges) for (const r of e.refs) if (r.file === path && r.line) {
        const other = n && e.source === n.id ? e.target : n && e.target === n.id ? e.source : (S.N.get(e.target).file === path ? e.source : e.target);
        if (!marks.has(r.line)) marks.set(r.line, []);
        marks.get(r.line).push({ e, other, r });
      }
      const lines = f.text.split('\n');
      wrap.innerHTML = `<div class="codehead"><b class="mono">${esc(path)}</b><span class="tag">${esc(f.lang)}</span><span class="muted">${f.binary ? `${f.size.toLocaleString()} bytes, binary: first 4 KB as hex` : `${lines.length.toLocaleString()} lines`}</span>
        <span style="flex:1"></span>${n ? `<a class="btn sm" href="${nodeHref(n.id)}">Show in estate map</a>` : ''}${n && n.type === 'program' ? `<a class="btn sm" href="${href('decompose', { n: n.id })}">Decompose</a>` : ''}<a class="btn sm" href="${GH}/blob/main/${esc(path)}" target="_blank" rel="noopener">GitHub</a></div>
        <pre class="code">${lines.map((l, i) => `<span class="l${marks.has(i + 1) ? ' ref' : ''}" id="L${i + 1}">${f.binary ? esc(l) : hl(l, f.lang)}</span>`).join('')}</pre>`;
      const fs = (S.findings.filter((x) => x.paths.includes(path)));
      const sorted = [...marks].sort((a, b) => a[0] - b[0]);
      refs.innerHTML = `<span class="eyebrow">In this file</span>
        <h3>Connections <span class="muted">(${sorted.reduce((a, [, v]) => a + v.length, 0)})</span></h3>
        ${sorted.length ? `<ul class="conn">${sorted.flatMap(([ln, v]) => v.map((m) => `<li><a class="ref" style="margin:0" href="${srcHref(path, ln)}">${ln}</a><span class="muted" style="font-size:12px">${esc(m.e.kind)}</span>${nodeChip(m.other)}</li>`)).join('')}</ul>` : '<p class="muted">No parsed references start in this file.</p>'}
        ${fs.length ? `<h3>Scan findings <span class="muted">(${fs.length})</span></h3><ul class="conn">${fs.map((x) => `<li>${sevTag(x.severity)}<a style="color:inherit" href="${href('findings', { id: x.id })}">${esc(x.title)}</a></li>`).join('')}</ul>` : ''}`;
    }
    wrap.querySelectorAll('.l.hit').forEach((x) => x.classList.remove('hit'));
    const t = line && wrap.querySelector('#L' + line);
    if (t) { t.classList.add('hit'); t.scrollIntoView({ block: 'center' }); } else if (!line) wrap.scrollTop = 0;
  }
  show(params);
  return { update: show };
}

// ------------------------------------------------------------ decompose ----
function findingBody(f) {
  const d = document.createElement('div');
  d.append(md(f.description));
  if (f.recommendation) d.append(h(`<h3>Recommendation</h3>`), md(f.recommendation));
  if (f.note) d.append(h(`<h3>Scan notes</h3>`), md(f.note));
  if (f.snippet) d.append(h(`<h3>Source excerpt</h3>`), h(`<pre class="prompt" style="white-space:pre;overflow:auto">${esc(f.snippet)}</pre>`));
  return d;
}
export function decomposeView(root, params) {
  const el = h(`<div class="split"><div class="list"><div class="head"><input type="search" placeholder="Filter programs…" aria-label="Filter programs">
    <div class="seg"><button class="btn sm on" data-v="">All</button><button class="btn sm" data-v="batch">Batch</button><button class="btn sm" data-v="online">Online</button></div></div><div class="items"></div></div><div class="detail"></div></div>`);
  root.append(el);
  const items = el.querySelector('.items'), detail = el.querySelector('.detail'), filter = el.querySelector('input');
  let sub = '', cur = null, tab = 'logic';
  function list() {
    const v = filter.value.trim().toUpperCase();
    items.innerHTML = S.units.filter((n) => (!sub || n.sub === sub) && (!v || n.label.includes(v))).map((n) => `<a class="item${n.id === cur ? ' on' : ''}" href="${href('decompose', { n: n.id })}">
      <div class="row"><b>${esc(n.label)}</b><span class="tag">${esc(n.sub)}</span>${(n.tech || []).filter((t) => t !== 'CICS').map((t) => `<span class="tag">${esc(t)}</span>`).join('')}</div>
      <small>${n.loc.toLocaleString()} lines · ${plural(n.findings, 'finding')}${n.rules ? ` · ${n.rules} rules` : ''}</small></a>`).join('');
  }
  filter.addEventListener('input', list);
  el.querySelector('.seg').addEventListener('click', (ev) => { const b = ev.target.closest('button'); if (!b) return; sub = b.dataset.v; el.querySelectorAll('.seg .btn').forEach((x) => x.classList.toggle('on', x === b)); list(); });
  function show(p) {
    const n = S.N.get(p.n)?.type === 'program' ? S.N.get(p.n) : S.N.get('pgm:CBACT04C');
    if (p.tab) tab = p.tab;
    cur = n.id; list();
    const fs = S.fPrimary.get(n.id)?.length ? S.fPrimary.get(n.id) : (S.fByNode.get(n.id) || []);
    const main = [...fs].sort((a, b) => (a.status === 'open' ? 0 : 1) - (b.status === 'open' ? 0 : 1) || b.description.length - a.description.length)[0];
    const rules = S.rulesByProg.get(n.id) || [];
    const o = S.out.get(n.id), i = S.inn.get(n.id), st = unitState(n);
    const names = (es, key) => es.map((e) => nodeChip(e[key])).join(' ') || '<span class="muted">none</span>';
    detail.innerHTML = `<span class="eyebrow">${esc(n.sub)} program · ${esc(n.module)}</span><h1>${esc(n.label)}</h1>
      <p class="lede" style="margin-bottom:10px">${esc(main ? main.title : 'No consolidated scan document for this program.')}</p>
      <div class="toolbar"><a class="btn" href="${srcHref(n.file)}">Open source</a><a class="btn" href="${nodeHref(n.id)}">Trace in estate map</a><a class="btn" href="${href('tracker', { n: n.id })}">Track</a>
        <span class="tag accent">${STAGES[st.stage].label}</span><span class="tag">${esc(APPROACH[n.approach.approach])}</span></div>
      <div class="three">
        <div class="card"><span class="eyebrow">Entry</span><p style="margin:6px 0 0">${names(i.filter((e) => ['runs', 'starts', 'calls', 'transfers'].includes(e.kind)), 'source')}</p></div>
        <div class="card"><span class="eyebrow">Reads</span><p style="margin:6px 0 0">${names(i.filter((e) => e.kind === 'reads'), 'source')}</p></div>
        <div class="card"><span class="eyebrow">Writes</span><p style="margin:6px 0 0">${names(o.filter((e) => e.kind === 'writes'), 'target')}</p></div>
        <div class="card"><span class="eyebrow">Calls and transfers</span><p style="margin:6px 0 0">${names(o.filter((e) => ['calls', 'transfers'].includes(e.kind)), 'target')}</p></div>
        <div class="card"><span class="eyebrow">Screens</span><p style="margin:6px 0 0">${names(o.filter((e) => e.kind === 'displays'), 'target')}</p></div>
        <div class="card"><span class="eyebrow">Record layouts</span><p style="margin:6px 0 0">${names(o.filter((e) => e.kind === 'includes'), 'target')}</p></div>
      </div>
      <div class="tabs" role="tablist"><button data-t="logic">Logic and flow</button><button data-t="rules">Business rules (${rules.length})</button><button data-t="findings">All findings (${(S.fByNode.get(n.id) || []).length})</button><button data-t="approach">Route</button></div>
      <div class="pane"></div>`;
    const pane = detail.querySelector('.pane');
    const paint = () => {
      detail.querySelectorAll('.tabs button').forEach((b) => b.classList.toggle('on', b.dataset.t === tab));
      pane.innerHTML = '';
      if (tab === 'logic') {
        if (!main) { pane.innerHTML = '<p class="muted">The scan produced no document whose subject is this program. Its connections above come from the source.</p>'; return; }
        pane.append(h(`<p class="muted" style="font-size:12px">From scan finding <a href="${href('findings', { id: main.id })}" class="mono">${main.id.slice(0, 14)}…</a> ${sevTag(main.severity)} <span class="tag ${main.status}">${main.status}</span></p>`), findingBody(main));
      } else if (tab === 'rules') {
        if (!rules.length) { pane.innerHTML = `<p class="muted">No rules catalog exists for ${esc(n.label)} yet. The catalog on branch <span class="mono">${esc(S.estate.meta.rulesBranch)}</span> covers ${[...new Set(S.rules.map((r) => r.program))].map((x) => `<a href="${href('decompose', { n: 'pgm:' + x, tab: 'rules' })}" class="mono">${x}</a>`).join(' and ')}. Characterizing this program is its next step in the tracker.</p>`; return; }
        const cols = rules[0].columns || [];
        const t = h(`<div class="tablewrap"><p class="muted" style="font-size:12px">From <a href="${GH}/blob/${esc(rules[0].branch)}/${esc(rules[0].doc)}" target="_blank" rel="noopener" class="mono">${esc(rules[0].doc)}</a> on branch ${esc(rules[0].branch)}. Rows marked "SME question" wait for a subject-matter expert.</p>
          <table><thead><tr><th>ID</th>${cols.map((c) => `<th>${esc(c)}</th>`).join('')}</tr></thead><tbody></tbody></table></div>`);
        for (const r of rules) {
          const tr = document.createElement('tr');
          tr.innerHTML = `<td class="mono">${esc(r.id)}${/SME question/i.test(r.cells.join(' ')) ? '<br><span class="tag accent">SME</span>' : ''}</td>`;
          r.cells.forEach((cell) => { const td = document.createElement('td'); const m = md(cell); m.style.maxWidth = '520px'; td.append(m); tr.append(td); });
          t.querySelector('tbody').append(tr);
        }
        pane.append(t);
      } else if (tab === 'findings') {
        const all = S.fByNode.get(n.id) || [];
        pane.innerHTML = all.length ? `<table><thead><tr><th>Severity</th><th>Finding</th><th>Category</th><th>Status</th></tr></thead><tbody>${all.map((f) => `<tr class="click" data-id="${f.id}"><td>${sevTag(f.severity)}</td><td>${esc(f.title)}</td><td class="mono">${esc(f.category)}</td><td><span class="tag ${f.status}">${f.status}</span></td></tr>`).join('')}</tbody></table>` : '<p class="muted">No findings reference this program.</p>';
        pane.querySelectorAll('tr.click').forEach((r) => r.addEventListener('click', () => go('findings', { id: r.dataset.id })));
      } else {
        const a = n.approach;
        pane.innerHTML = `<div class="note">Proposed by rule from the source structure and scan findings. An architect and SME confirm it before work starts.</div>
          <h2>${esc(APPROACH[a.approach])}</h2><p><b style="font-weight:500">Target:</b> ${esc(a.target)}</p><ul>${a.why.map((w) => `<li>${esc(w)}</li>`).join('')}</ul>
          <p><b style="font-weight:500">Alternative:</b> ${esc(a.alternative)}</p>
          <p>Relative complexity ${a.complexity}/100 (${a.effort})</p><div class="bar" style="max-width:320px"><i style="width:${a.complexity}%"></i></div>
          <h3>Next prompt for Devin</h3><div class="prompt">${esc(nextPrompt(n))}</div><button class="btn primary" id="cp">Copy prompt</button>`;
        pane.querySelector('#cp').addEventListener('click', () => copy(nextPrompt(n), 'Prompt copied. Paste it into Devin.'));
      }
    };
    detail.querySelector('.tabs').addEventListener('click', (ev) => { const b = ev.target.closest('button'); if (b) { tab = b.dataset.t; paint(); } });
    paint(); detail.scrollTop = 0;
    items.querySelector('.item.on')?.scrollIntoView({ block: 'nearest' });
  }
  show(params);
  return { update: show };
}

// ------------------------------------------------------------- findings ----
export function findingsView(root, params) {
  const cats = [...d3.rollup(S.findings, (v) => v.length, (f) => f.category)].sort((a, b) => b[1] - a[1]);
  const el = h(`<div class="split" style="grid-template-columns:420px 1fr"><div class="list"><div class="head">
      <input type="search" placeholder="Search ${S.findings.length} findings…" aria-label="Search findings">
      <div style="display:flex;gap:6px;flex-wrap:wrap"><select id="sev" aria-label="Severity"><option value="">Any severity</option>${['critical', 'high', 'medium', 'low'].map((s) => `<option>${s}</option>`).join('')}</select>
      <select id="stat" aria-label="Status"><option value="">Any status</option><option>open</option><option>dismissed</option></select>
      <select id="cat" aria-label="Category" style="max-width:150px"><option value="">Any category</option>${cats.map(([c, n]) => `<option value="${esc(c)}">${esc(c)} (${n})</option>`).join('')}</select></div>
      <small class="muted" id="count"></small></div><div class="items"></div></div><div class="detail"></div></div>`);
  root.append(el);
  const items = el.querySelector('.items'), detail = el.querySelector('.detail');
  const [qi, sev, stat, cat] = ['input', '#sev', '#stat', '#cat'].map((s) => el.querySelector(s));
  const order = { critical: 0, high: 1, medium: 2, low: 3 };
  let cur = null;
  function list() {
    const v = qi.value.trim().toLowerCase();
    const res = S.findings.filter((f) => (!sev.value || f.severity === sev.value) && (!stat.value || f.status === stat.value) && (!cat.value || f.category === cat.value) && (!v || (f.title + ' ' + f.description).toLowerCase().includes(v)))
      .sort((a, b) => (a.status === 'open' ? 0 : 1) - (b.status === 'open' ? 0 : 1) || order[a.severity] - order[b.severity] || a.title.localeCompare(b.title));
    el.querySelector('#count').textContent = `${res.length} of ${S.findings.length} · ${res.filter((f) => f.status === 'open').length} open`;
    items.innerHTML = res.map((f) => `<a class="item${f.id === cur ? ' on' : ''}" href="${href('findings', { id: f.id })}"><div class="row">${sevTag(f.severity)}<span class="tag ${f.status}">${f.status}</span><span class="mono muted" style="font-size:11px">${esc(f.category)}</span></div><div style="margin-top:3px">${esc(f.title)}</div></a>`).join('') || '<p class="muted" style="padding:12px">No findings match these filters.</p>';
    return res;
  }
  [qi, sev, stat, cat].forEach((x) => x.addEventListener('input', list));
  function show(p) {
    const res = list();
    const f = S.fById.get(p.id) || res[0];
    if (!f) { detail.innerHTML = ''; return; }
    cur = f.id; items.querySelectorAll('.item').forEach((a) => a.classList.toggle('on', a.getAttribute('href').endsWith(f.id)));
    detail.innerHTML = `<span class="eyebrow">${esc(f.category)} · ${esc(f.created)} UTC</span><h1 style="font-size:22px">${esc(f.title)}</h1>
      <div class="toolbar">${sevTag(f.severity)}<span class="tag ${f.status}">${f.status}</span><span class="mono muted">${f.id}</span></div>
      ${f.status === 'dismissed' ? '<div class="note">Dismissed during scan triage, usually because it was merged into a consolidated document for the same component. Kept here so nothing the scan produced is hidden.</div>' : ''}
      <p>${f.nodes.slice(0, 14).map(nodeChip).join(' ')}</p>
      ${f.paths.length ? `<p class="muted" style="font-size:12px">Files: ${f.paths.slice(0, 10).map((x) => `<a class="mono" href="${srcHref(x)}">${esc(x)}</a>`).join(', ')}</p>` : ''}`;
    detail.append(findingBody(f));
    if (f.owners) detail.append(h(`<p class="muted" style="font-size:12px;margin-top:20px">Code owners from git history: ${esc(f.owners)}</p>`));
    detail.scrollTop = 0;
    items.querySelector('.item.on')?.scrollIntoView({ block: 'nearest' });
  }
  show(params);
  return { update: show };
}
