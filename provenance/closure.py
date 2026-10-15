"""Static dependency floor: from entry-point .m files at a commit, follow identifiers
that name .m files present in the tree at that commit. A floor, not a census."""
import subprocess, re, sys, os
repo, commit = sys.argv[1], sys.argv[2]
entries = sys.argv[3:]
ls = subprocess.run(['git','-C',repo,'ls-tree','-r','--name-only',commit],capture_output=True,text=True).stdout.split('\n')
SKIP = ('depr/','old/','archive/','test','sandbox')
NEVER = ('spikes/help/',)
byname = {}
for p in ls:
    if p.startswith(NEVER): continue
    if '/@' in p or p.startswith('@') or p.startswith('cvx/') or '/private/' in p: continue
    if p.endswith('.m') or p.endswith('.mlapp'):
        n = os.path.splitext(os.path.basename(p))[0]
        # package functions: +pkg/fn.m -> pkg.fn
        m = re.search(r'\+(\w+)/(\w+)\.m$', p)
        if m: n = m.group(1)+'.'+m.group(2)
        byname.setdefault(n, []).append(p)
def pick(n):
    c = byname.get(n, [])
    c = sorted(c, key=lambda p:(any(s in p.lower() for s in SKIP), p.count('/'), p))
    return c[0] if c else None
def text(p):
    raw = subprocess.run(['git','-C',repo,'show',f'{commit}:{p}'],capture_output=True).stdout
    if p.endswith('.mlapp'):
        import zipfile, io
        try:
            z = zipfile.ZipFile(io.BytesIO(raw)); return z.read('matlab/document.xml').decode('utf8','ignore')
        except Exception: return ''
    return raw.decode('utf8','ignore')
seen, todo = {}, list(entries)
while todo:
    p = todo.pop()
    if p in seen: continue
    t = text(p); seen[p] = True
    t = re.sub(r'%.*', '', t)  # drop comments
    for tok in set(re.findall(r'\b([A-Za-z]\w*(?:\.\w+)?)\b', t)):
        q = pick(tok) or pick(tok.split('.')[0])
        if q and q not in seen: todo.append(q)
for p in sorted(seen): print(p)
