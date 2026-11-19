// Estate map: every artifact and connection parsed from the source, with animated upstream/downstream tracing.
import { S, TYPES, KINDS, STAGES, APPROACH, esc, h, go, href, srcHref, nodeHref, plural, unitState, nextPrompt, copy, sevTag } from './core.js';

const INK = '#141414', BLUE = '#2600FF', MUTED = '#7D7D7D', BG = '#FCFCFC';
const ANCHOR = {
  external: [-1180, -80], job: [-760, -330], transaction: [-760, 380], program: [-240, 0],
  screen: [170, 560], dataset: [330, -260], copybook: [860, 120],
};
const COLUMNS = [[-1180, 'PLATFORM SERVICES'], [-760, 'ENTRY POINTS'], [-240, 'PROGRAMS'], [330, 'DATA STORES + SCREENS'], [860, 'COPYBOOKS']];
const reduced = matchMedia('(prefers-reduced-motion: reduce)').matches;
let layoutCache = null;

function layout() {
  if (layoutCache) return layoutCache;
  const nodes = S.estate.nodes.map((n) => {
    const [ax, ay] = ANCHOR[n.type];
    const ay2 = n.type === 'program' ? (n.sub === 'online' ? 340 : -340) : ay;
    return { id: n.id, n, ax, ay: ay2, x: ax + (hash(n.id) % 200) - 100, y: ay2 + (hash(n.id + 'y') % 400) - 200, r: 4.5 + Math.sqrt(n.in + n.out) * 1.7 };
  });
  const byId = new Map(nodes.map((d) => [d.id, d]));
  const links = S.estate.edges.map((e, i) => ({ e, i, source: byId.get(e.source), target: byId.get(e.target), ph: (hash(e.source + e.target) % 1000) / 1000 }));
  const sim = d3.forceSimulation(nodes)
    .force('link', d3.forceLink(links).distance(110).strength(0.035))
    .force('charge', d3.forceManyBody().strength(-190).distanceMax(600))
    .force('x', d3.forceX((d) => d.ax).strength(0.42))
    .force('y', d3.forceY((d) => d.ay).strength(0.11))
    .force('collide', d3.forceCollide((d) => d.r + 11))
    .stop();
  for (let i = 0; i < 420; i++) sim.tick();
  for (const l of links) {  // control point for a gentle curve
    const dx = l.target.x - l.source.x, dy = l.target.y - l.source.y, len = Math.hypot(dx, dy) || 1;
    const bend = Math.min(60, len * 0.14) * (l.i % 2 ? 1 : -1);
    l.cx = (l.source.x + l.target.x) / 2 - (dy / len) * bend; l.cy = (l.source.y + l.target.y) / 2 + (dx / len) * bend;
  }
  return (layoutCache = { nodes, links, byId });
}
function hash(s) { let x = 2166136261; for (let i = 0; i < s.length; i++) { x ^= s.charCodeAt(i); x = Math.imul(x, 16777619); } return x >>> 0; }
const qpt = (l, t) => { const u = 1 - t; return [u * u * l.source.x + 2 * u * t * l.cx + t * t * l.target.x, u * u * l.source.y + 2 * u * t * l.cy + t * t * l.target.y]; };

function glyph(c, type, x, y, r, on, color) {
  c.beginPath();
  c.lineWidth = 1.4;
  c.strokeStyle = color; c.fillStyle = on ? color : BG;
  if (type === 'program') { c.arc(x, y, r, 0, 6.2832); c.fillStyle = color; c.fill(); }
  else if (type === 'job') { c.rect(x - r * 0.9, y - r * 0.9, r * 1.8, r * 1.8); c.fillStyle = color; c.fill(); }
  else if (type === 'transaction') { c.moveTo(x + r * 1.15, y); c.lineTo(x - r * 0.8, y - r); c.lineTo(x - r * 0.8, y + r); c.closePath(); c.fillStyle = color; c.fill(); }
  else if (type === 'screen') { c.rect(x - r * 1.3, y - r * 0.85, r * 2.6, r * 1.7); c.fill(); c.stroke(); }
  else if (type === 'copybook') { c.moveTo(x, y - r * 1.15); c.lineTo(x + r * 1.15, y); c.lineTo(x, y + r * 1.15); c.lineTo(x - r * 1.15, y); c.closePath(); c.fill(); c.stroke(); }
  else if (type === 'dataset') { c.arc(x, y, r, 0, 6.2832); c.fill(); c.lineWidth = 2.2; c.stroke(); c.beginPath(); c.arc(x, y, Math.max(1, r * 0.35), 0, 6.2832); c.fillStyle = color; c.fill(); }
  else { c.setLineDash([2, 2]); c.arc(x, y, r, 0, 6.2832); c.fill(); c.stroke(); c.setLineDash([]); }
}
function legendSvg(type) {
  const s = { program: '<circle cx="8" cy="8" r="5" fill="#141414"/>', job: '<rect x="3.5" y="3.5" width="9" height="9" fill="#141414"/>',
    transaction: '<path d="M13 8 4 3v10z" fill="#141414"/>', screen: '<rect x="2" y="4" width="12" height="8" fill="none" stroke="#141414" stroke-width="1.4"/>',
    copybook: '<path d="M8 2l6 6-6 6-6-6z" fill="none" stroke="#141414" stroke-width="1.4"/>',
    dataset: '<circle cx="8" cy="8" r="5" fill="none" stroke="#141414" stroke-width="2"/><circle cx="8" cy="8" r="1.6" fill="#141414"/>',
    external: '<circle cx="8" cy="8" r="5" fill="none" stroke="#141414" stroke-width="1.4" stroke-dasharray="2 2"/>' }[type];
  return `<svg width="16" height="16" viewBox="0 0 16 16" aria-hidden="true">${s}</svg>`;
}

export function mapView(root, params) {
  const { nodes, links, byId } = layout();
  const el = h(`<div class="map">
    <div class="stage">
      <canvas aria-label="Estate dependency graph"></canvas>
      <div class="hud">
        <div style="position:relative"><input type="search" id="q" placeholder="Find a program, job, file, screen…" style="width:300px" autocomplete="off" aria-label="Find a component"><div class="suggest" hidden></div></div>
        <div class="seg" id="dir" role="group" aria-label="Trace direction"><button class="btn" data-v="up">Upstream</button><button class="btn" data-v="both">Both</button><button class="btn" data-v="down">Downstream</button></div>
        <div class="seg" id="depth" role="group" aria-label="Trace depth"><button class="btn" data-v="1">1 hop</button><button class="btn" data-v="2">2</button><button class="btn" data-v="3">3</button><button class="btn" data-v="99">All</button></div>
        <button class="btn" id="fit">Fit</button><button class="btn" id="replay" hidden>Replay trace</button><button class="btn" id="clear" hidden>Clear</button>
      </div>
      <div class="title"><span class="eyebrow">CardDemo estate</span><h1>${nodes.length} components, ${links.length} connections</h1><p>Parsed from the source: COPY, CALL, EXEC CICS, EXEC SQL, JCL DD and Control-M conditions. Click anything to trace it.</p></div>
      <div class="legend" aria-label="Legend and filters"></div>
      <div class="tip" hidden></div>
    </div>
    <aside class="side" aria-live="polite"></aside>
  </div>`);
  root.append(el);
  const canvas = el.querySelector('canvas'), c = canvas.getContext('2d'), stage = el.querySelector('.stage'), side = el.querySelector('.side'), tip = el.querySelector('.tip');
  const st = { sel: null, dir: params.dir || 'both', depth: +(params.d || 1), hover: null, hideT: new Set(), hideK: new Set(), t: d3.zoomIdentity, trace: null, t0: performance.now(), intro: performance.now() };
  let W = 0, H = 0, dpr = 1, raf = 0, alive = true;

  // ---- legend doubles as type / connection filters ----
  const legend = el.querySelector('.legend');
  const tc = d3.rollup(nodes, (v) => v.length, (d) => d.n.type), kc = d3.rollup(links, (v) => v.length, (l) => l.e.kind);
  legend.innerHTML = Object.keys(TYPES).map((t) => `<button data-t="${t}" aria-pressed="true">${legendSvg(t)}${TYPES[t].plural}<span class="n">${tc.get(t) || 0}</span></button>`).join('') + '<details><summary>Connection kinds</summary>' +
    Object.keys(KINDS).filter((k) => kc.get(k)).map((k) => `<button data-k="${k}" aria-pressed="true"><svg width="16" height="16" aria-hidden="true"><path d="M1 8h14" stroke="#141414" ${k === 'includes' ? 'stroke-dasharray="2 2"' : ''}/></svg>${k}<span class="n">${kc.get(k)}</span></button>`).join('') + '</details>';
  legend.addEventListener('click', (ev) => {
    const b = ev.target.closest('button'); if (!b) return;
    const set = b.dataset.t ? st.hideT : st.hideK, key = b.dataset.t || b.dataset.k;
    set.has(key) ? set.delete(key) : set.add(key);
    b.classList.toggle('off', set.has(key)); b.setAttribute('aria-pressed', String(!set.has(key)));
    computeTrace(true);
  });

  // ---- zoom / pan ----
  const zoom = d3.zoom().scaleExtent([0.15, 6]).on('zoom', (ev) => { st.t = ev.transform; });
  const sel = d3.select(canvas).call(zoom).on('dblclick.zoom', null);
  function resize() {
    dpr = devicePixelRatio || 1; W = stage.clientWidth; H = stage.clientHeight;
    canvas.width = W * dpr; canvas.height = H * dpr;
  }
  function fit(ids, ms = 700) {
    const pts = (ids ? [...ids].map((i) => byId.get(i)) : nodes).filter(Boolean);
    if (!pts.length) return;
    const x0 = d3.min(pts, (d) => d.x) - 60, x1 = d3.max(pts, (d) => d.x) + 60, y0 = d3.min(pts, (d) => d.y) - 70, y1 = d3.max(pts, (d) => d.y) + 60;
    const k = Math.min(1.5, 0.9 * Math.min(W / (x1 - x0), H / (y1 - y0)));
    const t = d3.zoomIdentity.translate(W / 2, H / 2).scale(k).translate(-(x0 + x1) / 2, -(y0 + y1) / 2);
    (reduced || !ms ? sel : sel.transition().duration(ms).ease(d3.easeCubicInOut)).call(zoom.transform, t);
  }

  // ---- trace ----
  const visibleLink = (l) => !st.hideK.has(l.e.kind) && !st.hideT.has(l.source.n.type) && !st.hideT.has(l.target.n.type);
  function computeTrace(keepCamera) {
    el.querySelector('#clear').hidden = el.querySelector('#replay').hidden = !st.sel; el.querySelector('.title').style.visibility = st.sel ? 'hidden' : '';
    if (!st.sel || !byId.has(st.sel)) { st.trace = null; renderSide(); if (!keepCamera) fit(); return; }
    const level = new Map([[st.sel, 0]]), elevel = new Map();
    const walk = (dirn) => {
      // each direction keeps its own visited set, so a node reached downstream is still expanded upstream
      let frontier = [st.sel]; const seen = new Set(frontier);
      for (let d = 1; d <= st.depth && frontier.length; d++) {
        const next = [];
        for (const id of frontier) for (const l of links) {
          if (!visibleLink(l)) continue;
          const from = dirn > 0 ? l.source.id : l.target.id, to = dirn > 0 ? l.target.id : l.source.id;
          if (from !== id) continue;
          if (!elevel.has(l.i)) elevel.set(l.i, d);
          if (!level.has(to)) level.set(to, d * dirn);
          if (!seen.has(to)) { seen.add(to); next.push(to); }
        }
        frontier = next;
      }
    };
    if (st.dir !== 'up') walk(1);
    if (st.dir !== 'down') walk(-1);
    st.trace = { level, elevel, max: d3.max([...elevel.values()]) || 0 };
    st.t0 = performance.now();
    renderSide();
    if (!keepCamera) fit(level.keys());
  }
  function select(id, opts = {}) {
    st.sel = id;
    history.replaceState(null, '', href('map', { n: id, dir: st.dir === 'both' ? null : st.dir, d: st.depth === 1 ? null : st.depth }));
    syncButtons(); computeTrace(opts.keepCamera);
  }
  function syncButtons() {
    el.querySelectorAll('#dir .btn').forEach((b) => b.classList.toggle('on', b.dataset.v === st.dir));
    el.querySelectorAll('#depth .btn').forEach((b) => b.classList.toggle('on', +b.dataset.v === st.depth));
  }
  el.querySelector('#dir').addEventListener('click', (ev) => { const b = ev.target.closest('button'); if (b) { st.dir = b.dataset.v; select(st.sel); } });
  el.querySelector('#depth').addEventListener('click', (ev) => { const b = ev.target.closest('button'); if (b) { st.depth = +b.dataset.v; select(st.sel); } });
  el.querySelector('#fit').addEventListener('click', () => fit(st.trace ? st.trace.level.keys() : null));
  el.querySelector('#clear').addEventListener('click', () => select(null));
  el.querySelector('#replay').addEventListener('click', () => { st.t0 = performance.now(); });

  // ---- search ----
  const q = el.querySelector('#q'), sug = el.querySelector('.suggest');
  q.addEventListener('input', () => {
    const v = q.value.trim().toUpperCase();
    const hits = v ? nodes.filter((d) => d.n.label.toUpperCase().includes(v) || (d.n.dsn || '').includes(v)).sort((a, b) => a.n.label.toUpperCase().indexOf(v) - b.n.label.toUpperCase().indexOf(v) || b.r - a.r).slice(0, 12) : [];
    sug.hidden = !hits.length;
    sug.innerHTML = hits.map((d) => `<button data-id="${esc(d.id)}"><span class="mono">${esc(d.n.label)}</span><span class="muted">${TYPES[d.n.type].label}</span></button>`).join('');
  });
  q.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') { sug.querySelector('button')?.click(); } if (ev.key === 'Escape') { sug.hidden = true; } });
  sug.addEventListener('click', (ev) => { const b = ev.target.closest('button'); if (b) { sug.hidden = true; q.value = ''; select(b.dataset.id); } });

  // ---- pointer ----
  function pick(ev) {
    const r = canvas.getBoundingClientRect(), [x, y] = st.t.invert([ev.clientX - r.left, ev.clientY - r.top]);
    let best = null, bd = Infinity;
    for (const d of nodes) {
      if (st.hideT.has(d.n.type)) continue;
      const dist = Math.hypot(d.x - x, d.y - y) - d.r;
      if (dist < bd) { bd = dist; best = d; }
    }
    return bd < 9 / st.t.k + 3 ? best : null;
  }
  canvas.addEventListener('mousemove', (ev) => {
    const d = pick(ev); st.hover = d ? d.id : null;
    canvas.classList.toggle('pointer', !!d);
    tip.hidden = !d;
    if (d) { const r = stage.getBoundingClientRect(); tip.style.left = ev.clientX - r.left + 'px'; tip.style.top = ev.clientY - r.top + 'px';
      tip.innerHTML = `${esc(d.n.label)}<small>${esc(TYPES[d.n.type].label)}${d.n.sub ? ' · ' + esc(d.n.sub) : ''} · ${d.n.in} in / ${d.n.out} out</small>`; }
  });
  canvas.addEventListener('mouseleave', () => { st.hover = null; tip.hidden = true; });
  canvas.addEventListener('click', (ev) => { const d = pick(ev); if (d) select(d.id, { keepCamera: ev.shiftKey }); else if (st.sel) select(null); });

  // ---- draw ----
  function draw(now) {
    if (!alive) return;
    raf = requestAnimationFrame(draw);
    const { k, x, y } = st.t, tr = st.trace, time = now / 1000;
    c.setTransform(dpr, 0, 0, dpr, 0, 0); c.clearRect(0, 0, W, H);
    c.translate(x, y); c.scale(k, k);
    const introP = reduced ? 1 : Math.min(1, (now - st.intro) / 1600);
    // column captions
    c.font = '500 11px "Geist Mono", monospace'; c.fillStyle = MUTED; c.textAlign = 'center'; c.globalAlpha = tr ? 0.25 : 0.9 * introP;
    for (const [cx, name] of COLUMNS) c.fillText(name, cx, -820);
    c.globalAlpha = 1;
    const prog = (lvl) => (reduced ? 1 : Math.max(0, Math.min(1, (now - st.t0 - (lvl - 1) * 420) / 520)));
    const hoverN = st.hover && !tr ? st.hover : null;
    // edges
    for (const l of links) {
      if (!visibleLink(l)) continue;
      const lvl = tr ? tr.elevel.get(l.i) : null;
      const hov = hoverN && (l.source.id === hoverN || l.target.id === hoverN);
      let p = 1;
      if (tr && lvl) { p = prog(lvl); c.strokeStyle = BLUE; c.globalAlpha = 0.85; c.lineWidth = 1.5 / Math.sqrt(k); }
      else if (hov) { c.strokeStyle = INK; c.globalAlpha = 0.8; c.lineWidth = 1.2 / Math.sqrt(k); }
      else { c.strokeStyle = INK; c.globalAlpha = (tr || hoverN ? 0.04 : 0.17) * introP; c.lineWidth = 0.8 / Math.sqrt(k); p = introP < 1 ? Math.max(0, Math.min(1, introP * 1.6 - l.ph * 0.6)) : 1; }
      if (p <= 0) continue;
      if (l.e.kind === 'includes') c.setLineDash([3, 3]);
      c.beginPath(); c.moveTo(l.source.x, l.source.y);
      if (p >= 1) c.quadraticCurveTo(l.cx, l.cy, l.target.x, l.target.y);
      else for (let s = 1; s <= 12; s++) { const [px, py] = qpt(l, (p * s) / 12); c.lineTo(px, py); }
      c.stroke(); c.setLineDash([]);
      // flow: dots travel source -> target
      if (reduced) continue;
      if (tr && lvl && p >= 1) {
        c.fillStyle = BLUE; c.globalAlpha = 1;
        for (let j = 0; j < 2; j++) { const [px, py] = qpt(l, (time * 0.32 + l.ph + j * 0.5) % 1); c.beginPath(); c.arc(px, py, 2.4 / Math.sqrt(k), 0, 6.2832); c.fill(); }
      } else if (!tr && introP >= 1 && (hov || l.i % 3 === 0)) {
        const [px, py] = qpt(l, (time * 0.12 + l.ph) % 1);
        c.fillStyle = hov ? BLUE : INK; c.globalAlpha = hov ? 1 : 0.32; c.beginPath(); c.arc(px, py, (hov ? 2.4 : 1.3) / Math.sqrt(k), 0, 6.2832); c.fill();
      }
    }
    // nodes
    c.textAlign = 'left';
    for (const d of nodes) {
      if (st.hideT.has(d.n.type)) continue;
      let a = introP, color = INK, label = k > 1.05 || d.n.in + d.n.out >= 11;
      const lvl = tr ? tr.level.get(d.id) : null;
      if (tr) {
        if (lvl == null) { a = 0.1; label = false; }
        else { a = lvl === 0 ? 1 : prog(Math.abs(lvl)) * 0.999 + 0.001; label = a > 0.5 && (tr.level.size < 70 || k > 0.9 || lvl === 0); color = lvl === 0 ? BLUE : INK; if (a < 0.5) a = 0.1; }
      } else if (hoverN) {
        const near = d.id === hoverN || links.some((l) => (l.source.id === hoverN && l.target.id === d.id) || (l.target.id === hoverN && l.source.id === d.id));
        a = near ? 1 : 0.15; label = near;
      }
      c.globalAlpha = a;
      const r = d.r * (lvl === 0 ? 1.25 : 1);
      glyph(c, d.n.type, d.x, d.y, r, false, color);
      if (lvl === 0) { const pulse = reduced ? 0 : (time * 0.8) % 1; c.beginPath(); c.arc(d.x, d.y, r + 5 + pulse * 14, 0, 6.2832); c.strokeStyle = BLUE; c.globalAlpha = 0.7 * (1 - pulse); c.lineWidth = 1.5 / Math.sqrt(k); c.stroke(); c.globalAlpha = a; }
      if (label) {
        const fs = Math.max(9, Math.min(13, 11 / Math.sqrt(k))) * (lvl === 0 ? 1.25 : 1);
        c.font = `${lvl === 0 ? 500 : 400} ${fs}px "Geist Mono", monospace`;
        const tw = c.measureText(d.n.label).width;
        c.fillStyle = BG; c.globalAlpha = a * 0.85; c.fillRect(d.x + r + 4, d.y - fs * 0.6, tw + 4, fs * 1.2);
        c.fillStyle = color; c.globalAlpha = a; c.fillText(d.n.label, d.x + r + 6, d.y + fs * 0.35);
      }
    }
    c.globalAlpha = 1;
  }

  // ---- side panel ----
  function tours() {
    const most = (type, kind, dirn) => nodes.filter((d) => d.n.type === type).map((d) => [d, (dirn === 'in' ? S.inn : S.out).get(d.id).filter((e) => e.kind === kind).length]).sort((a, b) => b[1] - a[1])[0];
    const [cpy, cn] = most('copybook', 'includes', 'in');
    const acct = nodes.find((d) => d.n.label === 'ACCTDATA.VSAM.KSDS');
    const list = [
      ['pgm:CBACT04C', 'both', 1, 'Monthly interest run', 'CBACT04C, the INTCALC job that runs it, and every file it reads and writes.'],
      [acct?.id, 'both', 1, 'Who touches the account master', `Every program that reads or writes ${acct?.n.label}.`],
      ['pgm:COSGN00C', 'down', 2, 'From sign-on outward', 'The sign-on program, the menus it transfers to, and the screens behind them.'],
      [cpy.id, 'up', 1, 'One layout, many programs', `${cpy.n.label} is copied into ${cn} programs: change it and all of them move.`],
      ['job:CLOSEFIL', 'down', 3, 'The scheduled batch chain', 'Control-M conditions from CLOSEFIL onward, and what each job runs.'],
    ].filter((t) => byId.has(t[0]));
    return `<h3>Guided traces</h3><div class="tours">${list.map((t) => `<button data-tour="${esc(t[0])}|${t[1]}|${t[2]}"><b>${t[3]}</b><span>${esc(t[4])}</span></button>`).join('')}</div>`;
  }
  function renderSide() {
    const n = st.sel && S.N.get(st.sel);
    if (!n) {
      const cnt = (t) => nodes.filter((d) => d.n.type === t).length;
      side.innerHTML = `<span class="eyebrow">Estate map</span><h2 style="word-break:normal">See what is connected, and how</h2>
        <p class="muted">Every shape is a real artifact in the repository and every line is a reference found in the source. Dots move in the direction control or data flows.</p>
        <div class="reach"><div class="card"><b>${cnt('program')}</b><span>programs</span></div><div class="card"><b>${cnt('job')}</b><span>batch jobs and procs</span></div><div class="card"><b>${cnt('dataset')}</b><span>data stores</span></div>
        <div class="card"><b>${cnt('transaction')}</b><span>CICS transactions</span></div><div class="card"><b>${cnt('screen')}</b><span>screen maps</span></div><div class="card"><b>${cnt('copybook')}</b><span>copybooks</span></div></div>
        ${tours()}
        <h3>How to read it</h3><p class="muted">Click a component to trace it. Upstream is what leads to it; downstream is what it reaches. Raise the hop count to follow the chain further. The legend switches types and connection kinds on and off.</p>`;
      return;
    }
    const tr = st.trace, by = d3.rollup([...tr.level.keys()].filter((i) => i !== n.id), (v) => v.length, (i) => S.N.get(i).type);
    const reach = [...by].sort((a, b) => b[1] - a[1]).slice(0, 6).map(([t, v]) => `<div class="card"><b>${v}</b><span>${TYPES[t].plural.toLowerCase()}</span></div>`).join('');
    const groupEdges = (list, key, arrow) => {
      const g = d3.group(list, (e) => e.kind);
      return [...g].map(([kind, es]) => `<h3>${arrow} ${esc(KINDS[kind])} <span class="muted">(${es.length})</span></h3><ul class="conn">${es.sort((a, b) => S.N.get(a[key]).label.localeCompare(S.N.get(b[key]).label)).map((e) => {
        const r = e.refs[0];
        return `<li><a class="nodelink" href="${nodeHref(e[key])}" data-go="${esc(e[key])}">${esc(S.N.get(e[key]).label)}</a><span class="muted" style="font-size:12px">${esc(TYPES[S.N.get(e[key]).type].label)}</span>${r ? `<a class="ref" href="${srcHref(r.file, r.line)}" title="${esc(r.detail || '')}">${esc(r.file.split('/').pop())}:${r.line}</a>` : ''}</li>`;
      }).join('')}</ul>`).join('');
    };
    const fs = S.fByNode.get(n.id) || [];
    const unit = n.type === 'program' ? unitState(n) : null;
    side.innerHTML = `<span class="eyebrow">${esc(TYPES[n.type].label)}${n.sub ? ' · ' + esc(n.sub) : ''}</span>
      <h2>${esc(n.label)}</h2>
      <div class="sub">${n.file ? `<a class="mono" href="${srcHref(n.file, n.line)}">${esc(n.file)}</a>` : esc(n.dsn || 'Referenced by the source, not defined in this repository')}${n.loc ? ` · ${n.loc.toLocaleString()} lines` : ''}${n.module ? `<br>${esc(n.module)}` : ''}${(n.tech || []).length ? ' · ' + n.tech.map(esc).join(', ') : ''}</div>
      <div class="actions">
        ${n.file ? `<a class="btn" href="${srcHref(n.file, n.line)}">Open source</a>` : ''}
        ${n.type === 'program' ? `<a class="btn" href="${href('decompose', { n: n.id })}">Decompose</a><a class="btn" href="${href('tracker', { n: n.id })}">Track</a><button class="btn primary" id="cp">Copy next Devin prompt</button>` : ''}
      </div>
      ${unit ? `<p style="margin:10px 0 0"><span class="tag accent">${STAGES[unit.stage].label}</span> <span class="tag">${esc(APPROACH[n.approach.approach])}</span> <span class="muted" style="font-size:12px">proposed route</span></p>` : ''}
      <h3>This trace reaches</h3>
      <div class="reach">${reach || '<span class="muted">Nothing in this direction at this depth.</span>'}</div>
      ${groupEdges(S.inn.get(n.id), 'source', '←')}${groupEdges(S.out.get(n.id), 'target', '→')}
      ${fs.length ? `<h3>Scan findings <span class="muted">(${fs.length})</span></h3><ul class="conn">${fs.slice(0, 8).map((f) => `<li>${sevTag(f.severity)}<a href="${href('findings', { id: f.id })}" style="color:inherit">${esc(f.title)}</a></li>`).join('')}</ul>` : ''}`;
    side.scrollTop = 0;
    side.querySelector('#cp')?.addEventListener('click', () => copy(nextPrompt(n), 'Prompt copied. Paste it into Devin.'));
  }
  side.addEventListener('click', (ev) => {
    const t = ev.target.closest('[data-tour]'), g = ev.target.closest('[data-go]');
    if (t) { const [id, dir, d] = t.dataset.tour.split('|'); st.dir = dir; st.depth = +d; select(id); }
    else if (g) { ev.preventDefault(); select(g.dataset.go); }
  });

  const ro = new ResizeObserver(() => { resize(); });
  ro.observe(stage);
  resize();
  st.sel = params.n && byId.has(params.n) ? params.n : null;
  syncButtons();
  fit(null, 0);
  computeTrace(false);
  document.fonts.ready.then(() => { raf = requestAnimationFrame(draw); });
  return {
    update(p) { const id = p.n && byId.has(p.n) ? p.n : null; if (p.dir) st.dir = p.dir; if (p.d) st.depth = +p.d; if (id !== st.sel) { st.sel = id; syncButtons(); computeTrace(false); } },
    destroy() { alive = false; cancelAnimationFrame(raf); ro.disconnect(); },
  };
}
