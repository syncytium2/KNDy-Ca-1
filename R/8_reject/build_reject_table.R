# build_reject_table.R
#
# Precompute script for custard-based dead-ROI rejection. Builds verdicts for
# both ROI and pensub data sources in a single run.
# Runnable with: Rscript 01-scripts/build_reject_table.R
# Run from the repo root; here::here() resolves all paths relative to the project root.
#
# Reads:   04-data/<DATA_DATE>/long_window_20_ROI/long_window_20_ROI_custard_<DATA_VERSION>.csv
#          04-data/<DATA_DATE>/long_window_20_pensub/long_window_20_pensub_custard_<DATA_VERSION>.csv
# Writes:  output/reject_table_custard_roi.csv     — verdict table (one row per slice × ROI)
#          output/slice_summary_custard_roi.csv    — one row per slice; ROI counts + rejection rate
#          output/reject_table_custard_pensub.csv
#          output/slice_summary_custard_pensub.csv

library(here)

DATA_DATE    <- "2026-05-23"
DATA_VERSION <- "20260523"

source(here::here("01-scripts", "load_raw.R"))
source(here::here("01-scripts", "rejection_rule.R"))

SOURCES <- c("ROI", "pensub")

for (src in SOURCES) {
  src_lower   <- tolower(src)
  in_path     <- here::here("04-data", DATA_DATE,
                             sprintf("long_window_20_%s", src),
                             sprintf("long_window_20_%s_custard_%s.csv", src, DATA_VERSION))
  out_verdict <- here::here("output", sprintf("reject_table_custard_%s.csv",  src_lower))
  out_summary <- here::here("output", sprintf("slice_summary_custard_%s.csv", src_lower))

  df      <- load_raw(in_path)
  verdict <- compute_rejection_verdict(df)

  # Guard: empty verdict means no eligible slices — write empty CSVs and skip to next source.
  if (nrow(verdict) == 0) {
    cat(sprintf("WARNING [%s]: verdict table is empty; writing empty CSVs and skipping.\n", src))
    write.csv(verdict,      out_verdict, row.names = FALSE)
    write.csv(data.frame(), out_summary, row.names = FALSE)
    next
  }

  # Write verdict table (logical columns kept as TRUE/FALSE for readability)
  write.csv(verdict, out_verdict, row.names = FALSE)

  # Coerce for aggregation (integer sum works; original verdict CSV is already written above)
  v_for_agg <- verdict
  v_for_agg$group_id    <- as.character(v_for_agg$group_id)
  v_for_agg$hik_present <- as.integer(v_for_agg$hik_present)
  v_for_agg$rejected    <- as.integer(v_for_agg$rejected)

  # Slice summary: aggregate per (run_id, group_id, slice_id, drug)
  BY <- c("run_id", "group_id", "slice_id", "drug")

  n_total <- aggregate(ROI         ~ run_id + group_id + slice_id + drug,
                       v_for_agg, function(x) length(unique(x)))
  names(n_total)[names(n_total) == "ROI"] <- "n_total_roi"

  n_hik <- aggregate(hik_present ~ run_id + group_id + slice_id + drug,
                     v_for_agg, sum)
  names(n_hik)[names(n_hik) == "hik_present"] <- "n_hik_roi"

  n_rej <- aggregate(rejected    ~ run_id + group_id + slice_id + drug,
                     v_for_agg, sum)
  names(n_rej)[names(n_rej) == "rejected"] <- "n_rejected"

  slice_summary <- merge(merge(n_total, n_hik, by = BY), n_rej, by = BY)
  slice_summary$pct_rejected <- sprintf("%.1f%%",
    100 * slice_summary$n_rejected / slice_summary$n_total_roi)

  # Diagnostic columns: per-slice counts of empty ROIs at each phase.
  # n_empty_hik is NA (not 0) for slices that had no hi-K treatment.
  empty_diag <- verdict %>%
    dplyr::group_by(run_id, group_id, slice_id, drug) %>%
    dplyr::summarise(
      n_empty_base = sum(base_empty, na.rm = TRUE),
      n_empty_drug = sum(drug_empty, na.rm = TRUE),
      n_empty_hik  = if (any(hik_present)) sum(hik_empty, na.rm = TRUE) else NA_integer_,
      .groups = "drop"
    )
  slice_summary <- merge(slice_summary, empty_diag, by = BY)

  # Sort by group_id (canonical order) then slice_id
  GROUPS <- c("DI", "OVX", "MALE", "ORX")
  slice_summary$group_id <- factor(slice_summary$group_id, levels = GROUPS)
  slice_summary <- slice_summary[order(match(slice_summary$group_id, GROUPS),
                                        slice_summary$slice_id), ]
  rownames(slice_summary) <- NULL

  # Write slice summary
  write.csv(slice_summary, out_summary, row.names = FALSE)

  # Print run summary
  n_slices   <- nrow(slice_summary)
  n_rois     <- sum(slice_summary$n_total_roi)
  n_rejected <- sum(slice_summary$n_rejected)
  pct        <- sprintf("%.1f%%", 100 * n_rejected / n_rois)
  cat(sprintf("%s: %d slices, %d ROIs, %d rejected (%s)\n",
              src, n_slices, n_rois, n_rejected, pct))
  cat("wrote:", out_verdict, "\n")
  cat("wrote:", out_summary, "\n")
}
