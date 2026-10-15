# Provenance — which code this is, and how that was established

No commit or tag in the development repository recorded "the code that produced the manuscript".
This tree reconstructs it stage by stage: each stage is taken from the development commit that
matches the date that stage ran on the manuscript's data. Those dates were read from the
analysis archive itself (read-only) on 2026-10-15.

The development repositories are `syncytium2/interface2` (MATLAB) and `syncytium2/fireflies`
(R). They are private, so the commit IDs below cannot be looked up from outside; they are given
so the tree can be rebuilt and checked.

## Data the manuscript used

- 30 senktide recordings, all present in the frozen analysis archive.
- Export: 5-minute windows (`short_window_5`), FAST and SLOW streams, `ROI` source, dated
  2026-05-23. An export dated 2026-05-22 holds the same rows and values in a different order.
- Inactive ROIs removed: the manuscript used the `_clean.csv` files written by stage 8.

## Stage → commit

| stage | repo | commit | date | basis |
|---|---|---|---|---|
| 1–3 import, ROIs, ΔF/F₀ | interface2 | `e2df03b3` | 2026-03-24 | The state of the code when detection began. These stages ran per recording when each was acquired (2024–2026), on whatever version was current then; that history is not reconstructed here. |
| 4 custard | interface2 | `e2df03b3` | 2026-03-24 | The archive records the custard run as 25-Mar-2026; this is the last commit before it (the next is 2026-03-27). |
| 5 slow | interface2 | `1cb1833d` ⚠ | 2026-04-13 | The archive records the slow run as 13-Apr-2026. Two commits are candidates, `3717ae40` (04-10) and `1cb1833d` (04-13). **Provisional** until the stored run time and the thresholds the slow run used settle it. |
| 6 CMS | interface2 | `50ad7d2e` ⚠ | 2026-05-07 | Commit message "final run!". **Provisional** until the archive's stored CMS date confirms it. |
| 7 export | interface2 | `66aae9aa` | 2026-05-23 | The commit that named the `short_window_5` mode; the export folder carries that name and date. |
| 8 inactive-ROI removal | fireflies | `47040f4` | 2026-05-24 | The commit that extended the removal to the 5-minute exports. |

The full file list, with each file's source commit and path, is
[`provenance/files.tsv`](provenance/files.tsv). Rebuild and compare with:

```bash
python3 provenance/build_tree.py <interface2 clone> <fireflies clone>
git status   # no changes = the tree matches the pins
```

The file list for each stage comes from following function names from its entry point
(`provenance/closure.py`), plus that stage's rows in the development repository's own file
manifest. **This is a floor, not a census**: a function called by a name built at run time, or
from an App Designer callback, can be missed. Only a run from this tree proves a stage complete.

## Authorship

The development repositories mark commits written with an AI coding assistant (Claude Code,
Anthropic) with a `Co-Authored-By` trailer. For every file in this tree, `provenance/files.tsv` names the
commit that last changed it and whether that commit carries the trailer: none do in stages 1–6
and 8; all three files in stage 7 do. A trailer records
who wrote a commit in that repository; it does not by itself describe how the manuscript's
analysis was done.

## The 0007 detection bug does not affect these data

See [DETECTION_FIX_2026-07.md](DETECTION_FIX_2026-07.md). All 30 recordings were checked
directly in the archive on 2026-10-15: every custard threshold is positive, and the slow outputs
at threshold factors 2, 3, 4 and 5 differ in every recording, which is the opposite of the bug's
signature. The check's own controls passed: it flagged a synthetic recording carrying the bug,
and passed a recording from the corrected archive.
