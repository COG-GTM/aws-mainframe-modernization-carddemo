/** Minimal browser 3270 emulator for the HTTP API (served at GET /). */
export const TERMINAL_HTML = `<!doctype html>
<html><head><meta charset="utf-8"><title>CardDemo 3270</title>
<style>
body{background:#111;color:#ddd;font-family:monospace;margin:16px}
#scr{position:relative;width:80ch;height:24.2em;line-height:1em;font-size:16px;background:#000;border:1px solid #444;padding:4px;white-space:pre}
.f{position:absolute;height:1em}
input.f{background:#002;border:0;border-bottom:1px solid #4a4;color:#6f6;font:inherit;padding:0;outline:none}
.BLUE{color:#59f}.RED{color:#f55}.PINK{color:#f7f}.GREEN{color:#5f5}.TURQUOISE{color:#5ff}.YELLOW{color:#ff5}.NEUTRAL{color:#fff}
.BRT{font-weight:bold}
#bar{margin-top:8px}button{margin:2px;font-family:monospace}
#status{margin-top:6px;color:#aaa}
</style></head><body>
<div id="scr"></div>
<div id="bar"><span id="tx"><input id="transid" size="5" placeholder="CC00" value="CC00"> <button data-start>Start</button> </span>
<button data-aid="ENTER">Enter</button><button data-aid="CLEAR">Clear</button>
${Array.from({ length: 12 }, (_, i) => `<button data-aid="PF${i + 1}">F${i + 1}</button>`).join('')}</div>
<div id="status"></div>
<script>
let term=null,state=null;
const scr=document.getElementById('scr'),status=document.getElementById('status');
const esc=s=>s.replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
function render(r){
  state=r;term=r.terminalId;scr.innerHTML='';
  if(r.screen.map===null){const d=document.createElement('div');d.className='f';d.style.top='4px';d.style.left='4px';d.textContent=r.screen.lines.join('\\n');scr.appendChild(d);}
  for(const f of r.screen.fields){
    const top=(f.row-1)+'em',left=(f.col-1)+'ch';
    if(f.protection==='UNPROT'){const i=document.createElement('input');i.className='f '+(f.color||'');i.name=f.name||'';i.maxLength=f.length;
      i.style.top='calc(4px + '+top+')';i.style.left='calc(4px + '+left+')';i.style.width=f.length+'ch';i.value=f.value.replace(/\\s+$/,'');
      if(f.intensity==='DRK')i.type='password';i.dataset.orig=i.value;scr.appendChild(i);}
    else if(f.intensity!=='DRK'){const s=document.createElement('span');s.className='f '+(f.color||'')+' '+f.intensity;s.style.top='calc(4px + '+top+')';s.style.left='calc(4px + '+left+')';s.innerHTML=esc(f.value);scr.appendChild(s);}
  }
  const inputs=[...scr.querySelectorAll('input')];
  const cur=r.screen.fields.find(f=>f.row===r.screen.cursor.row&&f.col===r.screen.cursor.col&&f.protection==='UNPROT');
  (inputs.find(i=>cur&&i.name===cur.name)||inputs[0]||document.getElementById('transid')).focus();
  status.textContent='terminal '+term+' | ran '+(r.programs.join(' -> ')||'-')+' | next transid '+(r.nextTransid||'(none)')+(r.abend?' | ABEND '+r.abend.code:'');
}
async function post(url,body){const res=await fetch(url,{method:'POST',headers:{'content-type':'application/json'},body:JSON.stringify(body)});const j=await res.json();if(!res.ok){status.textContent='error: '+j.error;return;}render(j);}
function send(aid){
  if(!state||!state.nextTransid){const t=document.getElementById('transid').value.trim();if(t)post('/api/transaction/'+encodeURIComponent(t),{terminalId:term,aid});return;}
  const fields={};for(const i of scr.querySelectorAll('input'))if(i.value!==i.dataset.orig)fields[i.name]=i.value;
  post('/api/terminal/'+term,{aid,fields});
}
document.querySelectorAll('[data-aid]').forEach(b=>b.onclick=()=>send(b.dataset.aid));
document.querySelector('[data-start]').onclick=()=>post('/api/transaction/'+encodeURIComponent(document.getElementById('transid').value.trim()),{terminalId:term});
document.addEventListener('keydown',e=>{
  if(e.key==='Enter'){e.preventDefault();send('ENTER');}
  else if(e.key==='Escape'){e.preventDefault();send('CLEAR');}
  else if(/^F([1-9]|1[0-2])$/.test(e.key)){e.preventDefault();send('PF'+e.key.slice(1));}
});
post('/api/transaction/CC00',{});
</script></body></html>`;
