# Third-party code

Everything under `matlab/<stage>/thirdparty/` was written by others. It is included unmodified, as
it was when the pipeline ran, and is **not** covered by this repository's MIT licence.

| folder | source | licence | used by |
|---|---|---|---|
| `CaImAn-MATLAB-master/` | CaImAn-MATLAB (Flatiron Institute), https://github.com/flatironinstitute/CaImAn-MATLAB | GPL-2 (`license.txt` alongside) | `GetSn`, `prctfilt` and small utilities: detection (stages 4–5); `read_file`: import (stages 1–2) |
| `arrayResize/` | MATLAB File Exchange, Simon Musall | BSD-style (`license.txt` alongside) | import, ROI selection |
| `spikes/` | MLspike, Deneux et al. 2016, https://github.com/MLspike/spikes | **no licence file**; provenance note alongside | `spk_autosigma`, the noise estimate custard thresholds on (stages 4–5) |
| `explor/+brick/` | xplor toolbox, https://github.com/XplorMatlab/xplor (required by MLspike) | **no licence file**; provenance note alongside | helpers called by `spk_autosigma` |

## Required but not included

Install these separately and put them on the MATLAB path:

| package | used by |
|---|---|
| NoRMCorre, https://github.com/flatironinstitute/NoRMCorre (GPL-2) | motion correction (stage 1) and ROI selection (stage 2) |
| Bio-Formats for MATLAB (`bfmatlab`), https://www.openmicroscopy.org/bio-formats/ | reading the raw `.ets` files (stages 1–2) |
