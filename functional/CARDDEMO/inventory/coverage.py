#!/usr/bin/env python3
"""Coverage proof for the POSTTRAN stream. Usage: coverage.py REPO_ROOT [STREAM_ROOT]  (default JOB:POSTTRAN)

Program reachability uses JCL / CSD / CALL / XCTL / LINK edges only. Scheduler edges (job -> job) are context: they
order jobs, they do not make a program part of another job's stream. Exit 1 if the identity does not hold.
"""
import sys
from collections import defaultdict
sys.argv += ['.'] * (2 - len(sys.argv))
import importlib.util, pathlib
spec = importlib.util.spec_from_file_location('cg', pathlib.Path(__file__).with_name('callgraph.py'))
cg = importlib.util.module_from_spec(spec); cg.ROOT = None
sys_argv = sys.argv[:]; sys.argv = [sys.argv[0], sys_argv[1]]; spec.loader.exec_module(cg); sys.argv = sys_argv
stream_root = sys.argv[2] if len(sys.argv) > 2 else 'JOB:POSTTRAN'

progs, edges = cg.all_edges()
graph = defaultdict(set)
for s, d, k, c in edges:
    if not k.startswith('SCHED'):
        graph[s].add(d)

def reach(start):
    seen, todo = set(), [start]
    while todo:
        n = todo.pop()
        for d in graph[n]:
            if d not in seen:
                seen.add(d); todo.append(d)
    return seen

roots = sorted({s for s, d, k, c in edges if s.startswith(('JOB:', 'TRAN:')) and not k.startswith('SCHED')})
stream = reach(stream_root) & set(progs)
others = set()
for r in roots:
    if r != stream_root:
        others |= reach(r) & set(progs)
cls = {}
for p in progs:
    cls[p] = ('STREAM' if p not in others else 'SHARED') if p in stream else ('OUT_OF_STREAM' if p in others else 'UNREACHABLE')
count = defaultdict(int)
for p in progs: count[cls[p]] += 1
N = len(progs)
print(f'PROGRAMS {N}  ROOTS {len(roots)} (jobs {sum(r.startswith("JOB:") for r in roots)}, transactions {sum(r.startswith("TRAN:") for r in roots)})')
print('IDENTITY {} = STREAM {} + SHARED {} + OUT_OF_STREAM {} + UNREACHABLE {} -> {}'.format(
    N, count['STREAM'], count['SHARED'], count['OUT_OF_STREAM'], count['UNREACHABLE'],
    N == sum(count.values())))
print('STREAM CLOSURE', stream_root, '->', sorted(reach(stream_root)))
for c in ('STREAM', 'SHARED', 'UNREACHABLE'):
    print(c, sorted(p for p in progs if cls[p] == c))
targets = {d for s, d, k, c in edges if not d.startswith(('JOB:', 'PROC:', 'TRAN:'))}
print('EXTERNAL (system/runtime, no source by design)', sorted(targets & cg.EXT_PROGRAMS))
absent = sorted(t for t in targets - set(progs) - cg.EXT_PROGRAMS if not t.startswith('UNRESOLVED:'))
print('ABSENT (referenced, no source in app/)', absent)
print('UNRESOLVED DYNAMIC', sorted({(s, d, c) for s, d, k, c in edges if d.startswith('UNRESOLVED:')}))
print('TABLE-RESOLVED', sorted({s for s, d, k, c in edges if k.endswith('_TABLE')}))
called = {d for s, d, k, c in edges if not k.startswith('SCHED')}
print('ORPHAN PROGRAMS (no incoming edge)', sorted(set(progs) - called))
sched_jobs = {d for s, d, k, c in edges if k.startswith('SCHED')}
print('JOBS NOT IN CA-7', sorted(r for r in roots if r.startswith('JOB:') and r not in sched_jobs))
sys.exit(0 if N == sum(count.values()) and stream_root in roots else 1)
