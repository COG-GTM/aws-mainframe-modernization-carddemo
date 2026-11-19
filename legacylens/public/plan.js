// Migration approach, tracker, compliance results and the evidence ledger.
import { S, TYPES, STAGES, APPROACH, GH, esc, h, go, href, srcHref, nodeHref, nodeChip, md, sevTag, unitState, nextPrompt, copy, plural, saveTracker, resetTracker, toast } from './core.js';

// ------------------------------------------------------------- approach ----
export function approachView(root) {
  const comps = S.estate.nodes.filter((n) => n.approach);
  const progs = comps.filter((n) => n.type === 'program');
  const by = d3.rollup(comps, (v) => v, (n) => n.approach.approach);
  const cnt = (a) => (by.get(a) || []).length;
  const routes = [
    ['refactor', 'Convert the logic to a modern language and keep its behaviour, proven against the original outputs.', 'Batch programs with clear file contracts and CICS programs whose screen handling separates from the rules.'],
    ['rewrite', 'Redesign the component for the target platform; the old code is the specification, not the template.', 'Modules bound to IMS or MQ, where the data and messaging model changes anyway.'],
    ['replace', 'Drop the bespoke code in favour of a commercial or government off-the-shelf product, or a standard library.', 'Sign-on, user administration, date and wait utilities.'],
    ['retire', 'Do not migrate; the target platform already does this.', 'Jobs that only define, load, back up or sort VSAM files.'],
  ];
  // where to start: batch programs first, cheapest and best-evidenced first
  const start = progs.filter((n) => n.approach.approach === 'refactor').map((n) => {
    const st = unitState(n);
    const score = (n.sub === 'batch' ? 40 : 0) + (n.rules ? 30 : 0) + st.stage * 8 + (100 - n.approach.complexity) * 0.4;
    const why = [n.sub === 'batch' ? 'batch: outputs can be compared byte for byte' : 'online: needs screen-flow tests', n.rules ? `${n.rules} rules already catalogued` : null, st.ev[3].length ? 'parity evidence in review' : null, `complexity ${n.approach.complexity}/100`].filter(Boolean);
    return { n, score, why, st };
  }).sort((a, b) => b.score - a.score).slice(0, 8);
  // focus areas, each computed from the graph or the scan
  const touch = S.estate.nodes.filter((n) => n.type === 'dataset').map((n) => [n, new Set([...S.out.get(n.id), ...S.inn.get(n.id)].map((e) => (e.source === n.id ? e.target : e.source)).filter((i) => i.startsWith('pgm:'))).size]).sort((a, b) => b[1] - a[1]).slice(0, 5);
  const cpy = S.estate.nodes.filter((n) => n.type === 'copybook').map((n) => [n, S.inn.get(n.id).filter((e) => e.kind === 'includes').length]).sort((a, b) => b[1] - a[1]).slice(0, 5);
  const sme = S.rules.filter((r) => /SME question/i.test(r.cells.join(' ')));
  const plat = progs.filter((n) => (n.tech || []).some((t) => ['IMS', 'MQ', 'DB2'].includes(t)));
  const openHi = S.findings.filter((f) => f.status === 'open' && ['critical', 'high'].includes(f.severity));
  const orphan = S.estate.nodes.filter((n) => n.in + n.out === 0);
  const chips = (list) => list.map(([n, v]) => `<li>${nodeChip(n.id)}<span class="ref muted">${v} programs</span></li>`).join('');
  const el = h(`<div class="page">
    <span class="eyebrow">Proposed approach</span><h1>Four routes, chosen per component</h1>
    <p class="lede">No single strategy fits a whole estate. Each of the ${comps.length} programs and jobs gets the route its structure supports. These are proposals derived by rule from the source and the scan; an architect and the system's SMEs confirm them.</p>
    <div class="grid" style="grid-template-columns:repeat(auto-fit,minmax(250px,1fr));margin-bottom:14px">${routes.map(([a, what, where]) => `<div class="card"><div class="kpi"><b>${cnt(a)}</b><span>components</span></div><h2 style="margin-top:8px">${APPROACH[a]}</h2><p style="margin:0 0 8px">${what}</p><p class="muted" style="margin:0;font-size:13px">${where}</p></div>`).join('')}</div>
    <div class="note"><b style="font-weight:500;color:var(--text)">Lift and shift (replatform)</b> is available for every component as an interim step: rehost the COBOL unchanged on an emulation runtime to leave the mainframe quickly, then apply the routes above behind stable interfaces. It is listed as the alternative on each component rather than as an end state.</div>

    <h2 style="margin-top:34px">Where to start</h2>
    <p class="lede">Start where equivalence is cheapest to prove. Ranked by: batch before online, rules already catalogued, existing evidence, then lowest complexity.</p>
    <div class="tablewrap"><table><thead><tr><th>#</th><th>Component</th><th>Why here</th><th>Stage</th><th></th></tr></thead><tbody>${start.map((s, i) => `<tr><td class="mono">${i + 1}</td><td>${nodeChip(s.n.id)}<br><span class="muted" style="font-size:12px">${s.n.loc} lines</span></td><td>${s.why.map(esc).join(' · ')}</td><td><span class="tag accent">${STAGES[s.st.stage].label}</span></td><td><a class="btn sm" href="${href('tracker', { n: s.n.id })}">Next step</a></td></tr>`).join('')}</tbody></table></div>

    <h2 style="margin-top:34px">Areas to focus on</h2>
    <div class="two">
      <div class="card"><h3 style="margin-top:0">Shared data stores</h3><p class="muted" style="font-size:13px;margin-top:0">Programs that share a file must move together or stay in sync during a parallel run.</p><ul class="conn">${chips(touch)}</ul></div>
      <div class="card"><h3 style="margin-top:0">Shared record layouts</h3><p class="muted" style="font-size:13px;margin-top:0">${S.estate.stats.comp3} packed-decimal (COMP-3) declarations across the source. Build the copybook codecs once, before any program.</p><ul class="conn">${chips(cpy)}</ul></div>
      <div class="card"><h3 style="margin-top:0">Questions only an SME can answer</h3><p class="muted" style="font-size:13px;margin-top:0">${sme.length} of ${S.rules.length} catalogued rules are flagged for a decision: preserve the behaviour or fix it.</p><ul class="conn">${sme.slice(0, 5).map((r) => `<li><a class="mono" href="${href('decompose', { n: 'pgm:' + r.program, tab: 'rules' })}">${esc(r.id)}</a><span style="font-size:13px">${esc(r.cells[1].replace(/\*\*|`/g, '').replace(/^SME question\.?\s*/i, '').slice(0, 110))}…</span></li>`).join('')}</ul></div>
      <div class="card"><h3 style="margin-top:0">Platform-bound modules</h3><p class="muted" style="font-size:13px;margin-top:0">${plat.length} programs use IMS, MQ or DB2. They need a target data and messaging design before conversion.</p><p>${plat.map((n) => nodeChip(n.id)).join(' ')}</p></div>
      <div class="card"><h3 style="margin-top:0">Open high and critical findings</h3><p class="muted" style="font-size:13px;margin-top:0">${openHi.length} open findings rated high or critical in the migration scan.</p><ul class="conn">${openHi.slice(0, 5).map((f) => `<li>${sevTag(f.severity)}<a style="color:inherit;font-size:13px" href="${href('findings', { id: f.id })}">${esc(f.title)}</a></li>`).join('')}</ul><p style="margin-bottom:0"><a href="${href('findings')}">All findings</a></p></div>
      <div class="card"><h3 style="margin-top:0">Likely dead code</h3><p class="muted" style="font-size:13px;margin-top:0">Artifacts with no parsed reference in or out. Confirm with the owner, then leave them behind.</p><p>${orphan.map((n) => nodeChip(n.id)).join(' ') || 'None found.'}</p></div>
    </div>

    <h2 style="margin-top:34px">Every component</h2>
    <div class="toolbar"><div class="seg" id="af"><button class="btn sm on" data-v="">All</button>${Object.keys(APPROACH).map((a) => `<button class="btn sm" data-v="${a}">${APPROACH[a]} (${cnt(a)})</button>`).join('')}</div></div>
    <div class="tablewrap"><table><thead><tr><th>Component</th><th>Type</th><th>Route</th><th>Target</th><th>Complexity</th><th>Findings</th></tr></thead><tbody id="rows"></tbody></table></div>
  </div>`);
  root.append(el);
  const rows = el.querySelector('#rows');
  const paint = (a) => { rows.innerHTML = comps.filter((n) => !a || n.approach.approach === a).sort((x, y) => y.approach.complexity - x.approach.complexity).map((n) => `<tr><td>${nodeChip(n.id)}</td><td>${esc(n.sub)}</td><td><span class="tag">${APPROACH[n.approach.approach]}</span></td><td style="max-width:420px">${esc(n.approach.target)}</td><td><div class="bar"><i style="width:${n.approach.complexity}%"></i></div><span class="mono muted">${n.approach.complexity} · ${n.approach.effort}</span></td><td class="mono">${n.findings}</td></tr>`).join(''); };
  el.querySelector('#af').addEventListener('click', (ev) => { const b = ev.target.closest('button'); if (!b) return; el.querySelectorAll('#af .btn').forEach((x) => x.classList.toggle('on', x === b)); paint(b.dataset.v); });
  paint('');
}

// -------------------------------------------------------------- tracker ----
export function trackerView(root, params) {
  const el = h(`<div class="page">
    <span class="eyebrow">Migration tracker</span><h1>Where each program stands, and what happens next</h1>
    <p class="lede">Stages follow the delivery gates. A stage counts once a pull request naming the program has been reviewed and merged. Those merges went into migration branches (<span class="mono">migration/carddemo-java</span>, <span class="mono">migration/ts</span>), not <span class="mono">main</span>, so nothing here is in production. Open pull requests and the unapproved rules catalog show as work in review (lighter bar). Change any row when you know better. Edits are saved for everyone using this app.</p>
    <div class="gates" id="gates"></div>
    <div class="toolbar"><input type="search" id="q" placeholder="Filter programs…" aria-label="Filter programs"><select id="fs" aria-label="Stage"><option value="">Any stage</option>${STAGES.map((s, i) => `<option value="${i}">${s.label}</option>`).join('')}</select>
      <span class="muted" id="sum"></span></div>
    <div class="tablewrap"><table><thead><tr><th>Program</th><th>Route</th><th>Stage</th><th>Evidence by stage</th><th>Owner</th><th>Next human gate</th></tr></thead><tbody id="rows"></tbody></table></div>
    <h2 style="margin-top:34px">Work already on the repository</h2>
    <p class="lede">${S.prs.length} pull requests exist on the CardDemo repository from earlier Devin runs (${S.prs.filter((p) => p.state === 'MERGED').length} merged, ${S.prs.filter((p) => p.state === 'OPEN').length} open). The repository is a shared demo, so several runs cover the same programs. Branches for board tickets CD-103 to CD-109:</p>
    <p>${['cd-103-classified-evidence', 'cd-104-golden-master', 'cd-105-posting-reject-handling', 'cd-106-copybook-records-codecs', 'cd-107-business-rules-catalog', 'cd-108-spring-batch-job', 'cd-109-interest-calculation'].map((b) => `<a class="tag" href="${GH}/tree/devin/${b}" target="_blank" rel="noopener">devin/${b}</a>`).join(' ')}</p>
  </div>`);
  root.append(el);
  const rows = el.querySelector('#rows'), q = el.querySelector('#q'), fs = el.querySelector('#fs');
  let drawer = null;
  function paint() {
    const all = S.units.map((n) => ({ n, st: unitState(n) }));
    el.querySelector('#gates').innerHTML = STAGES.map((s, i) => `<div class="gate"><b>${all.filter((u) => u.st.stage === i).length}</b>${s.label}<em>Gate: ${s.gate}</em></div>`).join('');
    const done = all.reduce((a, u) => a + u.st.stage, 0), max = all.length * 4;
    el.querySelector('#sum').textContent = `${all.length} programs · ${Math.round((done / max) * 100)}% of stage steps complete · ${all.filter((u) => u.st.manual).length} edited by hand`;
    const v = q.value.trim().toUpperCase();
    rows.innerHTML = all.filter((u) => (!v || u.n.label.includes(v)) && (fs.value === '' || u.st.stage === +fs.value)).map(({ n, st }) => `<tr class="click${params.n === n.id ? ' sel' : ''}" data-id="${n.id}" tabindex="0" role="button" aria-label="Open ${esc(n.label)}">
      <td><b class="mono" style="font-weight:500">${esc(n.label)}</b><br><span class="muted" style="font-size:12px">${esc(n.sub)} · ${n.loc} lines</span></td>
      <td><span class="tag">${APPROACH[n.approach.approach]}</span></td>
      <td><div class="stagebar" title="${STAGES[st.stage].label}">${STAGES.map((_, i) => `<i class="${i <= st.stage ? 'on' : !st.manual && i <= st.review ? 'rev' : ''}"></i>`).join('')}</div><span style="font-size:12px">${STAGES[st.stage].label}${st.manual ? ' <span class="muted">(edited)</span>' : st.review > st.stage ? ` <span class="muted">· ${STAGES[st.review].label.toLowerCase()} in review</span>` : ''}</span></td>
      <td style="font-size:12px" class="muted">${st.ev.map((e, i) => (e.length ? `${STAGES[i].label}: ${e.filter((x) => x.passed).length} passed, ${e.filter((x) => !x.passed).length} in review` : '')).filter(Boolean).join('<br>')}</td>
      <td>${esc(st.owner) || '<span class="muted">unassigned</span>'}</td><td style="font-size:13px">${st.stage < 4 ? esc(STAGES[st.stage + 1].gate) : 'Complete'}</td></tr>`).join('') || '<tr><td colspan="6" class="muted">No programs match.</td></tr>';
  }
  function open(id) {
    drawer?.remove(); drawer = null;
    const n = S.N.get(id); if (!n || n.type !== 'program') return;
    const st = unitState(n);
    const prs = S.prByNode.get(n.id) || [];
    drawer = h(`<aside class="drawer" role="dialog" aria-label="${esc(n.label)} tracker detail">
      <div style="display:flex;justify-content:space-between;align-items:start;gap:10px"><div><span class="eyebrow">${esc(n.sub)} program</span><h2 style="font-size:24px">${esc(n.label)}</h2></div><button class="btn" id="x">Close</button></div>
      <p><a href="${nodeHref(n.id)}">Trace in estate map</a> · <a href="${href('decompose', { n: n.id })}">Decomposition</a> · <a href="${srcHref(n.file)}">Source</a></p>
      <h3>Stage</h3><div class="seg" id="stg">${STAGES.map((s, i) => `<button class="btn sm${i === st.stage ? ' on' : ''}" data-v="${s.id}">${s.label}</button>`).join('')}</div>
      <p class="muted" style="font-size:12px">${st.manual ? `Set by hand${st.updated ? ' on ' + new Date(st.updated).toLocaleString() : ''}. Evidence alone supports "${STAGES[st.derived].label}". <a href="#" id="reset">Return to evidence</a>` : 'Derived from evidence: the highest stage with a pull request merged into a migration branch. Pick a stage to override it.'}</p>
      <div class="two"><label>Owner<br><input type="text" id="owner" value="${esc(st.owner)}" placeholder="Name or team" style="width:100%"></label><span></span></div>
      <label style="display:block;margin-top:8px">Note<br><textarea id="note" rows="2" style="width:100%" placeholder="Decisions, blockers, SME answers">${esc(st.note)}</textarea></label>
      <button class="btn" id="save" style="margin-top:6px">Save owner and note</button>
      <h3>Next prompt for Devin</h3><div class="prompt">${esc(nextPrompt(n))}</div><button class="btn primary" id="cp">Copy prompt</button>
      <h3>Evidence</h3>${st.ev.map((e, i) => (e.length ? `<p style="margin:4px 0"><span class="tag">${STAGES[i].label}</span> ${e.map((x) => esc(x.t)).join('; ')}</p>` : '')).join('') || '<p class="muted">None yet.</p>'}
      ${prs.length ? `<h3>Pull requests naming ${esc(n.label)} <span class="muted">(${prs.length})</span></h3><ul class="conn">${prs.map((p) => `<li><a href="${esc(p.url)}" target="_blank" rel="noopener" class="mono">#${p.number}</a><span style="font-size:13px">${esc(p.title)}</span><span class="ref tag ${p.state === 'MERGED' ? 'green' : ''}">${p.state.toLowerCase()}</span></li>`).join('')}</ul>` : ''}
    </aside>`);
    document.body.append(drawer);
    drawer.querySelector('#x').focus();
    const refresh = () => { if (!el.isConnected) return; paint(); if (drawer?.isConnected) open(id); };
    drawer.querySelector('#x').addEventListener('click', () => { drawer.remove(); drawer = null; history.replaceState(null, '', href('tracker')); params = {}; paint(); });
    drawer.querySelector('#stg').addEventListener('click', async (ev) => { const b = ev.target.closest('button'); if (!b) return; try { await saveTracker(id, { stage: b.dataset.v }); toast('Stage saved'); refresh(); } catch { toast('Could not save. Try again.'); } });
    drawer.querySelector('#reset')?.addEventListener('click', async (ev) => { ev.preventDefault(); const t = S.tracker.get(id); if (t.owner || t.note) await saveTracker(id, { stage: null }); else await resetTracker(id); toast('Back to the evidence-derived stage'); refresh(); });
    drawer.querySelector('#save').addEventListener('click', async () => { try { await saveTracker(id, { owner: drawer.querySelector('#owner').value, note: drawer.querySelector('#note').value }); toast('Saved'); refresh(); } catch { toast('Could not save. Try again.'); } });
    drawer.querySelector('#cp').addEventListener('click', () => copy(nextPrompt(n), 'Prompt copied. Paste it into Devin.'));
  }
  rows.addEventListener('click', (ev) => { const r = ev.target.closest('tr.click'); if (r) go('tracker', { n: r.dataset.id }); });
  rows.addEventListener('keydown', (ev) => { if (ev.key === 'Enter' || ev.key === ' ') { const r = ev.target.closest('tr.click'); if (r) { ev.preventDefault(); go('tracker', { n: r.dataset.id }); } } });
  q.addEventListener('input', paint); fs.addEventListener('input', paint);
  paint();
  if (params.n) open(params.n);
  return { update(p) { params = p; paint(); if (p.n) open(p.n); else { drawer?.remove(); drawer = null; } }, destroy() { drawer?.remove(); } };
}

// ----------------------------------------------------------- compliance ----
const FAM = { AC: 'Access control', AU: 'Audit and accountability', CM: 'Configuration management', IA: 'Identification and authentication', SA: 'System and services acquisition', SC: 'System and communications protection', SI: 'System and information integrity', SR: 'Supply chain risk management' };
export function complianceView(root, params) {
  const C = S.compliance, F = C.findings;
  const fam = Object.keys(FAM).map((k) => [k, F.filter((f) => f.families.includes(k))]);
  const max = d3.max(fam, (x) => x[1].length);
  const dev = F.filter((f) => f.category === 'deviation'), open = F.filter((f) => f.status === 'open');
  const el = h(`<div class="page" style="max-width:1400px">
    <span class="eyebrow">Compliance scan · a second COBOL estate</span><h1>Control gaps found in COBOL source</h1>
    <p class="lede">The same scan engine, pointed at a different question. This run checked <span class="mono">${esc(C.repo)}</span> (Enterprise COBOL, CICS, DB2, VSAM and JCL investment-portfolio programs) against ${C.catalogControls} code-owned NIST SP 800-53 Rev 5 High-baseline controls.</p>
    <div class="note">This is not the CardDemo repository shown in the rest of the app, and no compliance scan has been run on CardDemo. Findings report implementation gaps with file and line evidence. They are inputs for the authorizing official; they do not state whether the system meets a control.</div>
    <div class="grid kpis"><div class="card kpi"><b>${F.length}</b><span>findings</span></div><div class="card kpi"><b>${dev.length}</b><span>deviations: evidence a control is not implemented in code</span></div><div class="card kpi"><b>${F.length - dev.length}</b><span>observations: evidence that bears on a control</span></div><div class="card kpi"><b>${open.length}</b><span>open after triage</span></div><div class="card kpi"><b>${new Set(F.flatMap((f) => f.controls)).size}</b><span>distinct controls cited</span></div></div>
    <div class="two" style="grid-template-columns:340px 1fr;align-items:start">
      <div><h3 style="margin-top:0">By control family</h3><div id="fam">${fam.map(([k, v]) => `<button class="item" data-f="${k}" style="all:unset;display:block;cursor:pointer;padding:6px 0;width:100%"><div style="display:flex;justify-content:space-between"><span><b class="mono" style="font-weight:500">${k}</b> ${FAM[k]}</span><span class="mono">${v.length}</span></div><div class="bar accent"><i style="width:${(v.length / max) * 100}%"></i></div></button>`).join('')}</div>
        <p style="margin-top:14px"><a href="${esc(C.url)}" target="_blank" rel="noopener">Open the scan in Devin</a></p></div>
      <div><div class="toolbar"><select id="cf" aria-label="Family"><option value="">All families</option>${Object.keys(FAM).map((k) => `<option value="${k}">${k} ${FAM[k]}</option>`).join('')}</select>
        <select id="cc" aria-label="Kind"><option value="">Deviations and observations</option><option>deviation</option><option>observation</option></select>
        <select id="cs" aria-label="Status"><option value="">Any status</option><option>open</option><option>dismissed</option></select><span class="muted" id="cn"></span></div>
        <div class="tablewrap"><table><thead><tr><th>Severity</th><th>Finding</th><th>Controls</th><th>Kind</th><th>Status</th></tr></thead><tbody id="rows"></tbody></table></div></div>
    </div></div>`);
  root.append(el);
  const [cf, cc, cs] = ['#cf', '#cc', '#cs'].map((s) => el.querySelector(s)), rows = el.querySelector('#rows');
  const order = { high: 0, medium: 1, low: 2 };
  let drawer = null;
  function paint() {
    const res = F.filter((f) => (!cf.value || f.families.includes(cf.value)) && (!cc.value || f.category === cc.value) && (!cs.value || f.status === cs.value)).sort((a, b) => (a.status === 'open' ? 0 : 1) - (b.status === 'open' ? 0 : 1) || order[a.severity] - order[b.severity]);
    el.querySelector('#cn').textContent = `${res.length} of ${F.length}`;
    rows.innerHTML = res.map((f) => `<tr class="click" data-id="${f.id}" tabindex="0" role="button"><td>${sevTag(f.severity)}</td><td>${esc(f.title)}</td><td class="mono" style="white-space:nowrap">${f.controls.slice(0, 3).map(esc).join(' ')}${f.controls.length > 3 ? ' +' + (f.controls.length - 3) : ''}</td><td>${f.category}</td><td><span class="tag ${f.status}">${f.status}</span></td></tr>`).join('') || '<tr><td colspan="5" class="muted">No findings match.</td></tr>';
  }
  function open_(id) {
    drawer?.remove(); const f = F.find((x) => x.id === id); if (!f) return;
    drawer = h(`<aside class="drawer" role="dialog" aria-label="Compliance finding"><div style="display:flex;justify-content:space-between;gap:10px;align-items:start"><span class="eyebrow">${f.category} · ${f.controls.map(esc).join(' ')}</span><button class="btn" id="x">Close</button></div>
      <h2 style="font-size:20px;margin-top:6px">${esc(f.title)}</h2><p>${sevTag(f.severity)} <span class="tag ${f.status}">${f.status}</span> <span class="mono muted">${f.id.slice(0, 14)}…</span></p><div id="b"></div></aside>`);
    const b = drawer.querySelector('#b');
    b.append(md(f.description));
    if (f.recommendation) b.append(h('<h3>Recommendation</h3>'), md(f.recommendation));
    for (const s of f.snippets) b.append(h(`<div><h3 class="mono">${esc(s.loc)}</h3>${s.commentary ? `<p class="muted" style="font-size:13px;margin:0">${esc(s.commentary)}</p>` : ''}<pre class="prompt" style="white-space:pre;overflow:auto">${esc(s.code)}</pre></div>`));
    if (f.note) b.append(h('<h3>Triage note</h3>'), md(f.note));
    document.body.append(drawer);
    drawer.querySelector('#x').focus();
    drawer.querySelector('#x').addEventListener('click', () => { drawer.remove(); drawer = null; rows.querySelector(`tr[data-id="${id}"]`)?.focus(); });
  }
  rows.addEventListener('click', (ev) => { const r = ev.target.closest('tr.click'); if (r) open_(r.dataset.id); });
  rows.addEventListener('keydown', (ev) => { if (ev.key === 'Enter' || ev.key === ' ') { const r = ev.target.closest('tr.click'); if (r) { ev.preventDefault(); open_(r.dataset.id); } } });
  el.querySelector('#fam').addEventListener('click', (ev) => { const b = ev.target.closest('[data-f]'); if (b) { cf.value = cf.value === b.dataset.f ? '' : b.dataset.f; paint(); } });
  [cf, cc, cs].forEach((x) => x.addEventListener('input', paint));
  paint();
  return { destroy() { drawer?.remove(); } };
}

// ------------------------------------------------------------- evidence ----
export function evidenceView(root) {
  const m = S.estate.meta, st = S.estate.stats, n = S.estate.nodes;
  const lines = Object.entries(st.linesByLang).sort((a, b) => b[1] - a[1]);
  const total = d3.sum(lines, (d) => d[1]);
  const open = S.findings.filter((f) => f.status === 'open').length;
  const row = (cls, claim, basis) => `<tr><td><span class="tag ${cls === 'Not demonstrated here' ? 'critical' : cls === 'Proposed' ? 'accent' : ''}">${cls}</span></td><td>${claim}</td><td class="muted">${basis}</td></tr>`;
  root.append(h(`<div class="page">
    <span class="eyebrow">Evidence ledger</span><h1>What this app shows, and how far it can be trusted</h1>
    <p class="lede">Every number and connection here is generated from the inputs below. Nothing is typed in by hand. Where something is a proposal or has not been run, it says so.</p>
    <div class="grid kpis">${lines.filter(([k]) => !['Text', 'Markdown'].includes(k)).slice(0, 6).map(([k, v]) => `<div class="card kpi"><b>${v.toLocaleString()}</b><span>lines of ${esc(k)}</span></div>`).join('')}</div>
    <p class="muted" style="margin-top:-14px">${total.toLocaleString()} source lines under <span class="mono">app/</span> in ${S.estate.files.length} tracked files, including comments and blank lines. This is a reference application, not a million-line estate; the scan fans out per component, so a larger estate adds parallel work rather than a different method.</p>
    <h2 style="margin-top:30px">Claims by class</h2>
    <div class="tablewrap"><table><thead><tr><th>Class</th><th>What the app shows</th><th>Basis</th></tr></thead><tbody>
      ${row('Repository-derived', `Estate map: ${n.length} components and ${S.estate.edges.length} connections, each with a file and line`, `Parsed from <span class="mono">${m.repo}</span> at commit <span class="mono">${m.commit}</span> by <span class="mono">legacylens/tools/build_data.py</span>. Static parsing: references built at run time from data can be missed.`)}
      ${row('Repository-derived', `Source browser: all ${S.estate.files.length} files`, 'Read from the repository checkout on request.')}
      ${row('Scan-derived', `${S.findings.length} migration findings (${open} open, ${S.findings.length - open} dismissed in triage) with flows, data and migration notes`, `Devin migration-docs Code Scan <span class="mono">${m.scan}</span>, profile "${esc(m.scanProfile)}". A scan documents what it read; it does not guarantee a complete inventory.`)}
      ${row('Scan-derived', `${S.compliance.findings.length} control-gap findings on a second COBOL repository`, `Devin compliance Code Scan <span class="mono">${S.compliance.scan}</span> on <span class="mono">${S.compliance.repo}</span>.`)}
      ${row('Repository-derived', `${S.rules.length} business rules for ${[...new Set(S.rules.map((r) => r.program))].join(' and ')}`, `Rules catalog on branch <span class="mono">${m.rulesBranch}</span>, written by Devin, awaiting SME approval.`)}
      ${row('Repository-derived', `${S.prs.length} pull requests, ${S.prs.filter((p) => p.nodes.some((x) => x.startsWith('pgm:'))).length} of them linked to a program`, 'GitHub pull requests on the repository, matched by program or job name in the title. A stage inferred this way is a starting point, not a sign-off.')}
      ${row('Reported, not re-verified', 'Board tickets CD-102 to CD-109', 'Named by the session that ran the scan. The matching branches exist on GitHub; the tickets themselves were not opened from this app.')}
      ${row('Proposed', 'Route per component, complexity score, where to start', 'Rules over the graph and findings (technology used, lines, coupling, finding severity). For an architect and SME to confirm.')}
      ${row('Proposed', 'Next prompt for Devin on each program', 'Generated from the program\'s stage and its parsed dependencies.')}
      ${row('Not demonstrated here', 'Natural / ADABAS and Fortran estates', 'This repository contains COBOL, JCL, BMS, CICS definitions, DB2, IMS, MQ and two Assembler modules. No Natural or Fortran source was scanned for this app. Running the same scan on such a repository would populate the same views; the parser behind the estate map would need rules for those languages.')}
      ${row('Not demonstrated here', 'Production cut-over', 'No component in this repository has been cut over. The tracker has the stage; nothing claims it.')}
    </tbody></table></div>
    <h2 style="margin-top:30px">How the work is gated</h2>
    <p class="lede">Devin does the work between gates. People decide at each one.</p>
    <div class="gates">${STAGES.map((s, i) => `<div class="gate"><span class="eyebrow">${i + 1}</span><b style="font-size:16px">${s.label}</b><em>Gate: ${s.gate}</em></div>`).join('')}</div>
  </div>`));
}
