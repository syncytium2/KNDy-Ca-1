"""Rebuild the release tree from interface2 and fireflies at the commits in pins.tsv.

  python3 provenance/build_tree.py <interface2 clone> <fireflies clone>

Each stage folder is self-contained: files come from that stage's pinned commit, found by
following .m names from its entry points (closure.py) plus the stage's rows in the
interface2 manifest. Third-party code keeps its original sub-path under <stage>/thirdparty/.
This is a floor, not a census: only a run proves a stage folder complete.
"""
import csv, os, subprocess, sys
here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
repos = {'interface2': sys.argv[1], 'fireflies': sys.argv[2]}
THIRD = ('CaImAn-MATLAB-master/', 'CNMF_E/', 'spikes/', 'explor/', 'arrayResize/')
MANIFEST_STAGE = {'1_import_moco': 'import_moco', '2_roi_manual': 'roi_manual', '3_dfof0': 'dfof0',
                  '4_custard': 'custard', '5_slow': 'slow', '6_cms': 'cms', '7_export': 'export'}
manifest = list(csv.DictReader(open(os.path.join(here, 'manifest_from_interface2_main_2026-09-17.tsv')), delimiter='\t'))
listing = open(os.path.join(here, 'files.tsv'), 'w')
listing.write('stage\tpath_in_release\tsource_repo\tsource_commit\tsource_path\tlast_changed_in\tai_coauthor_trailer\n')
for pin in csv.DictReader(open(os.path.join(here, 'pins.tsv')), delimiter='\t'):
    repo, commit, stage = repos[pin['repo']], pin['commit'], pin['stage']
    entries = pin['entry_points'].split()
    if pin['repo'] == 'interface2':
        files = subprocess.run([sys.executable, '-I', os.path.join(here, 'closure.py'), repo, commit, *entries],
                               capture_output=True, text=True, check=True).stdout.split()
        tree = set(subprocess.run(['git', '-C', repo, 'ls-tree', '-r', '--name-only', commit],
                                  capture_output=True, text=True, check=True).stdout.split('\n'))
        for row in manifest:
            if row['owner'] == 'external' or MANIFEST_STAGE[stage] not in row['stages'].split(','):
                continue
            rel = row['file'].split('/interface2/', 1)[-1]
            if rel in tree and rel not in files:
                files.append(rel)
    else:
        files = []
        for e in entries:
            if e.endswith('/'):
                files += [f for f in subprocess.run(['git', '-C', repo, 'ls-tree', '-r', '--name-only', commit, e],
                                                   capture_output=True, text=True, check=True).stdout.split('\n') if f]
            else:
                files.append(e)
    keep_layout = any(e.endswith('/') for e in entries)
    if pin['repo'] == 'interface2':
        for lic in ('CaImAn-MATLAB-master/license.txt', 'arrayResize/license.txt'):
            if lic in tree and any(f.startswith(lic.split('/')[0] + '/') for f in files):
                files.append(lic)
    for src in sorted(set(files)):
        third = src.startswith(THIRD)
        if pin['repo'] == 'fireflies':
            dst = os.path.join('R', stage, src if keep_layout else os.path.basename(src))
        elif third:
            dst = os.path.join('matlab', stage, 'thirdparty', src)
        else:
            dst = os.path.join('matlab', stage, src)
        blob = subprocess.run(['git', '-C', repo, 'show', f'{commit}:{src}'], capture_output=True, check=True).stdout
        os.makedirs(os.path.join(root, os.path.dirname(dst)), exist_ok=True)
        open(os.path.join(root, dst), 'wb').write(blob)
        last = subprocess.run(['git', '-C', repo, 'log', '-1', '--format=%h', commit, '--', src],
                              capture_output=True, text=True, check=True).stdout.strip()
        body = subprocess.run(['git', '-C', repo, 'show', '-s', '--format=%B', last],
                              capture_output=True, text=True, check=True).stdout.lower()
        trailer = 'yes' if 'co-authored-by: claude' in body else 'no'
        listing.write(f'{stage}\t{dst}\t{pin["repo"]}\t{commit}\t{src}\t{last}\t{trailer}\n')
# Provenance notes for third-party code that shipped without a licence file.
for stage in sorted(os.listdir(os.path.join(root, 'matlab'))):
    for pkg in ('spikes', 'explor'):
        d = os.path.join(root, 'matlab', stage, 'thirdparty', pkg)
        if os.path.isdir(d):
            open(os.path.join(d, 'PROVENANCE.md'), 'w').write(open(os.path.join(here, 'notes', pkg + '.md')).read())
