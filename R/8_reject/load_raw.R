# load_raw.R
#
# Canonical data loader for FAST/SLOW (and custard) CSVs.
# Sourced by both the Shiny app (05-app/app-plot-nested-stats.R) and the
# precompute scripts (build_reject_table_fastslow.R, build_reject_table_custard.R).
#
# load_raw(path) reads a CSV, drops mouse_id == 81, canonicalizes treatment
# strings, enforces the MIN_DURATION_S floor, and runs two load-time integrity
# checks (EMPTY/EVENTS invariant; dead-slice detection). These checks are
# intentional guards — they stop execution with a diagnostic message if the
# input data violates expected structure.

suppressPackageStartupMessages(library(dplyr))

load_raw <- function(path) {
  df <- read.csv(path, head = TRUE)
  df <- df[df$mouse_id != 81, ]
  df <- df %>% dplyr::mutate(treatment = dplyr::case_when(
    grepl("^ba", treatment, ignore.case = TRUE) ~ "baseline",
    grepl("^se", treatment, ignore.case = TRUE) ~ "senktide",
    grepl("^tt", treatment, ignore.case = TRUE) ~ "TTX",
    grepl("^sb", treatment, ignore.case = TRUE) ~ "SB222200",
    grepl("^hi", treatment, ignore.case = TRUE) ~ "high K+",
    grepl("^wa", treatment, ignore.case = TRUE) ~ "wash",
    TRUE ~ NA_character_
  ))
  na_tx <- unique(df$treatment[is.na(df$treatment)])
  if (length(na_tx) > 0) {
    stop("Unrecognized treatment strings after normalization: ",
         paste(na_tx, collapse = ", "))
  }
  MIN_DURATION_S <- 300   # exclude recordings shorter than 5 minutes (hi-K exempt)
  df <- df[df$treatment == "high K+" | df$duration >= MIN_DURATION_S, ]
  # Integrity: every row must be exactly EMPTY (freq == 0, n_events == 0,
  # both metrics NA) or EVENTS (freq > 0, n_events > 0, both metrics non-NA).
  is_empty_r  <- df$freq == 0  & df$n_events == 0 &
                  is.na(df$amp_median) &  is.na(df$width_median)
  is_events_r <- df$freq >  0  & df$n_events >  0 &
                 !is.na(df$amp_median) & !is.na(df$width_median)
  viol_idx    <- which(!(is_empty_r | is_events_r))
  if (length(viol_idx) > 0L) {
    diag_df <- df[head(viol_idx, 20L),
                  c("slice_id", "ROI", "treatment",
                    "n_events", "freq", "amp_median", "width_median")]
    stop(
      sprintf("%d row(s) violate the EMPTY/EVENTS invariant.\n", length(viol_idx)),
      paste(capture.output(print(diag_df, row.names = FALSE)), collapse = "\n")
    )
  }
  df
}
