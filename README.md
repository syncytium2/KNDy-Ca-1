# KNDy-Ca-1

The MATLAB pipeline, the R step that removes inactive ROIs, and the R Shiny app that drew the
before/after plots, behind the calcium-event analysis
in the KNDy manuscript: import and motion correction, hand-drawn ROIs, ΔF/F₀, event detection
(custard, slow, and FAST = custard minus slow), export to CSV, inactive-ROI removal, and before/after plots.

The manuscript reports the **FAST** and **SLOW** streams, using **5-minute windows** (baseline,
then senktide after a 2-minute solution delay), from **senktide recordings only**.

> **This tree is the code as it ran for the published numbers**, not the current code. Each stage
> is taken from the commit that matches the date it ran on the manuscript's data
> ([PROVENANCE.md](PROVENANCE.md)). A later fix to the detection code (July 2026) did **not**
> affect the manuscript's data; [DETECTION_FIX_2026-07.md](DETECTION_FIX_2026-07.md) records
> what it was and why it does not apply.

> **Status: a record, not a turnkey package.** These are the files that ran, unedited. The tree
> has not been run end to end in this layout, so a helper function may be missing from a stage
> folder, and the folder paths are those of the original machines.

## Layout

Each stage folder is self-contained, because different stages ran on different versions of
shared files (`custardWrapper3.m`, for one, differs between stages 4 and 5). Put **one stage
folder** on the MATLAB path at a time:

```matlab
addpath(genpath('matlab/4_custard'))
```

| stage | folder | entry point | reads → writes |
|---|---|---|---|
| 1 | `matlab/1_import_moco` | `aCa0_TDtrawler.m` | raw `.ets` → motion-corrected stacks and projections (`complete` files) |
| 2 | `matlab/2_roi_manual` | `aCa3_ROIs.mlapp` (App Designer) | projections → hand-drawn ROIs |
| 3 | `matlab/3_dfof0` | `aCa4_ROI_dFoF0.m`, then `aCa4a_convert2indie.m` | ROIs + stacks → per-recording ΔF/F₀ traces |
| 4 | `matlab/4_custard` | `aCa9a_fast_batch_autothreshold.m` | ΔF/F₀ → custard events, with a per-ROI threshold from a noise estimate |
| 5 | `matlab/5_slow` | `aCa9a_slow_batch_v3.m` | ΔF/F₀ + stage-4 thresholds → slow events |
| 6 | `matlab/6_cms` | `aCa9z_custard_minus_slow.m` | custard and slow events → FAST (custard minus slow) |
| 7 | `matlab/7_export` | `aCa5z_batch_fast_slow_mlspike_v4.m`, `run_mode = "short_window_5"` | events + experiment timing → per-ROI CSVs |
| 8 | `R/8_reject` | `build_reject_table.R`, then `apply_reject_table.R` | CSVs → `_clean.csv` files with inactive ROIs removed |
| 9 | `R/9_shiny_app` | `shiny::runApp("05-app/app-baseline-sig.R")`, run from that folder | `_clean.csv` → the before/after plots |

Stage 8 builds its verdict from the **20-minute** custard export (stage 7 with
`run_mode = "long_window_20"`) and applies it to the 5-minute FAST and SLOW files. Its loader
also drops rows shorter than its minimum duration, which is how one recording with a
45-second senktide window was excluded.

## Statistics

The statistical analysis of the `_clean` files was done by Amanda Gibson, in her own repository:
**https://github.com/gibson-amandag/ArcKissCa_analysis**. The 5-minute senktide analysis is in
`03-analysis-notebooks/CombinedAnalysis_5minSenktide.Rmd` and
`CombinedAnalysis_5minSenktide_tables.Rmd`. That code is hers and is not copied here.

## Requirements

- MATLAB. The current pipeline is run on R2025b; the versions these stages originally ran on are
  not recorded here. Toolbox requirements have not yet been confirmed by a clean run.
- NoRMCorre and Bio-Formats for MATLAB, not included ([THIRD_PARTY.md](THIRD_PARTY.md)).
- R with the `here` package for stage 8; stage 9 also needs `shiny`, `shinyFiles` and the
  packages `01-scripts/00-load-packages.R` installs. Copy `Renviron.example` to `.Renviron` in
  `R/9_shiny_app` and set its paths.
- The experiment-timing workbook stage 7 reads (`exp_timing` sheet) is lab metadata and is not
  included.

## Paths

The code is unedited, so it still contains the folder paths of the machines it ran on
(`uigetdir` defaults, network-storage paths, `if2_paths.m` defaults). Point these at your own
data before running. `if2_paths.m` (stage 7) honours the `IF2_DATA_ROOT` and `IF2_CSVOUT_ROOT`
environment variables.

## Licence

MIT for our code ([LICENSE](LICENSE)). Third-party code keeps its own licence or carries a
provenance note ([THIRD_PARTY.md](THIRD_PARTY.md)).
