# Detection fix, July 2026 — what it was, and why it does not apply to the manuscript

## The bug

Slow detection (stage 5) does not set its own thresholds: it reuses the per-ROI thresholds that
custard (stage 4) stored, and keeps only peaks above them.

On **2026-04-13** (development commit `1cb1833d`) the automatic-threshold branch of
`custardWrapper3.m` was disabled by mistake. The branch tested for `0`, but automatic mode is
signalled by `-1`, so the branch never ran and every ROI's threshold was stored as `-1`. With a
threshold of `-1` every peak passes, so slow detection kept nearly everything, and its runs at
threshold factors 2, 3, 4 and 5 came out identical. Nothing raised an error.

A second problem hid the first fix: `custardWrapper3` runs inside `parfor`, and an already-open
MATLAB parallel pool keeps the compiled copy of a function, so after the code was corrected an
open pool went on running the old version.

## The fix

**2026-07-09 / 2026-07-10**, development commits `761ce40`, `dea6cdc` and `5a7bfbf`:

- the automatic-threshold branch was restored;
- an assertion now stops a run if every threshold comes back `-1`;
- both batch scripts restart the parallel pool before running.

## Why the manuscript's data are unaffected

Only recordings whose custard step was run between 2026-04-13 and the fix carry the bug. All 30
recordings in the manuscript had custard run on **2026-03-25**, before the bug existed. Checked in
the archive on 2026-10-15:

- no recording has thresholds of `-1` in any custard threshold source;
- slow event totals at factors 2, 3, 4 and 5 differ in every recording.

So stage 4 in this tree, taken from 2026-03-24, contains the working automatic-threshold branch,
and the numbers it produced are the ones the manuscript reports.

## Which code to use for new work

The current development code carries the fix and later changes. It does **not** reproduce the
manuscript's event counts exactly: on the 20-minute custard export, the corrected code produced
about 8% fewer events on the same recordings and windows. That difference has not been explained;
it is not the bug above, since these recordings never carried it. Use this tree to reproduce the
manuscript.
