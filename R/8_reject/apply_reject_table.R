# apply_reject_table.R
#
# Applies custard-derived rejection verdicts to short- and long-window senktide CSVs
# for both ROI and pensub data sources, producing 16 _clean.csv files.
# Runnable with: Rscript 01-scripts/apply_reject_table.R
# Run from the repo root; here::here() resolves all paths relative to the project root.
#
# Reads:   output/reject_table_custard_roi.csv
#          output/reject_table_custard_pensub.csv
#          04-data/<DATA_DATE>/<window>_<source>/<window>_<source>_<algo>_<DATA_VERSION>.csv  (16 files)
# Writes:  same path with _clean.csv suffix  (16 files)

library(here)

DATA_DATE    <- "2026-05-23"
DATA_VERSION <- "20260523"

source(here::here("01-scripts", "load_raw.R"))

SOURCES <- c("ROI", "pensub")
ALGOS   <- c("custard", "FAST", "SLOW", "MLspike")
WINDOWS <- c("short_window_5", "long_window_20")

for (src in SOURCES) {
  src_lower <- tolower(src)
  verdict   <- read.csv(here::here("output",
                                    sprintf("reject_table_custard_%s.csv", src_lower)),
                        stringsAsFactors = FALSE)
  verdict$rejected <- as.logical(verdict$rejected)
  rej      <- verdict[verdict$rejected, c("slice_id", "ROI")]
  rej_key  <- paste(rej$slice_id, rej$ROI, sep = "||")
  cat(sprintf("\n[%s] verdict: %d (slice_id, ROI) pairs rejected\n", src, length(rej_key)))

  for (win in WINDOWS) {
    src_dir <- here::here("04-data", DATA_DATE, sprintf("%s_%s", win, src))

    for (algo in ALGOS) {
      in_path  <- file.path(src_dir,
                            sprintf("%s_%s_%s_%s.csv",       win, src, algo, DATA_VERSION))
      out_path <- file.path(src_dir,
                            sprintf("%s_%s_%s_%s_clean.csv", win, src, algo, DATA_VERSION))

      n_raw   <- nrow(read.csv(in_path, stringsAsFactors = FALSE))
      df      <- load_raw(in_path)
      n_filt  <- nrow(df)
      row_key <- paste(df$slice_id, df$ROI, sep = "||")
      df_out  <- df[!row_key %in% rej_key, ]
      n_out   <- nrow(df_out)
      write.csv(df_out, out_path, row.names = FALSE)
      cat(sprintf("  %s %s %s: raw=%d filtered=%d clean=%d\n",
                  src, win, algo, n_raw, n_filt, n_out))
    }
  }
}
