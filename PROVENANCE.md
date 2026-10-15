# Provenance — which code this is, and how that was established

No commit or tag in the development repository recorded "the code that produced the manuscript".
This tree reconstructs it stage by stage: each stage is taken from the development commit that
matches when, and with what settings, that stage ran on the manuscript's data. Those run times
and settings were read from the analysis archive itself (read-only) on 2026-10-15. Times are
the wall clock of the machine that ran the code; the archive stores no time zone.

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
| 4 custard | interface2 | `e2df03b3` | 2026-03-24 | The archive records the custard runs on 25-Mar-2026, 17:34–23:44. This is the last commit before them; the next is 2026-03-27. |
| 5 slow | interface2 | `1cb1833d` | 2026-04-13 | The archive records the slow runs on 13-Apr-2026, 12:05–18:23, and every recording stores a `revised_threshold` identical to its custard thresholds. That reuse first appears in `1cb1833d` and is absent from the previous version (`3717ae40`); the slow script did not change again until July. The runs began about three hours before the commit time (15:01), so the earliest recordings ran on the working copy that was then committed. |
| 6 CMS | interface2 | `50ad7d2e` | 2026-05-07 | The archive stores no CMS run date. It does store the settings: custard at threshold factor 2 minus slow `filtfilt` at factor 2. Only two versions select factor 2: `e5bb8125`, a test set to 30 files starting at file 30, and `50ad7d2e` ("final run!"), set to all files. Matched by settings, not by date. |
| 7 export | interface2 | `66aae9aa` | 2026-05-23 | The commit that named the `short_window_5` mode; the export folder carries that name and date. |
| 8 inactive-ROI removal | fireflies | `47040f4` | 2026-05-24 | The commit that extended the removal to the 5-minute exports. |
| 9 Shiny app (before/after plots) | fireflies | `2966490` | 2026-05-28 | The last change to the app or its plotting functions before July, so any plot exported between then and the submission came from this version. Inferred from that date, not from a stamp on the published figures. |

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
and 8; all three files in stage 7 do, and 8 of the 39 files in stage 9. A trailer records
who wrote a commit in that repository; it does not by itself describe how the manuscript's
analysis was done.

## The 0007 detection bug does not affect these data

See [DETECTION_FIX_2026-07.md](DETECTION_FIX_2026-07.md). All 30 recordings were checked
directly in the archive on 2026-10-15: every custard threshold is positive, and the slow outputs
at threshold factors 2, 3, 4 and 5 differ in every recording, which is the opposite of the bug's
signature. The check's own controls passed: it flagged a synthetic recording carrying the bug,
and passed a recording from the corrected archive.
