import { S, loadAll, route, esc } from './core.js';
import { mapView } from './map.js';
import { sourceView, decomposeView, findingsView } from './explore.js';
import { approachView, trackerView, complianceView, evidenceView } from './plan.js';

const VIEWS = { map: mapView, source: sourceView, decompose: decomposeView, findings: findingsView, approach: approachView, tracker: trackerView, compliance: complianceView, evidence: evidenceView };
const root = document.getElementById('view');
let current = null;

async function render() {
  const r = route();
  const view = VIEWS[r.view] ? r.view : 'map';
  document.querySelectorAll('#nav a').forEach((a) => a.classList.toggle('on', a.dataset.view === view));
  // a view can keep its DOM and just react to new params (the map does, so the camera does not reset)
  if (current && current.name === view && current.update) { current.update(r.params); return; }
  current?.destroy?.();
  root.innerHTML = '';
  root.scrollTop = 0;
  current = { name: view, ...(VIEWS[view](root, r.params) || {}) };
}

loadAll().then(() => {
  const m = S.estate.meta;
  document.getElementById('repoTag').textContent = `${m.repo} @ ${m.commit}`;
  document.getElementById('footMeta').textContent = `Built from the repository source at ${m.commit} and migration-docs scan ${m.scan.slice(0, 13)}… (${S.findings.length} findings)`;
  window.addEventListener('hashchange', render);
  render();
}).catch((e) => { root.innerHTML = `<p class="loading">Could not load the estate data: ${esc(e.message)}</p>`; });
