# rejection_rule.R
#
# Custard-based dead-ROI rejection module.
# Source this file to load compute_rejection_verdict().
#
# compute_rejection_verdict(df)
#   Input:  data frame as returned by load_raw() — single dataset (custard).
#   Output: verdict table, one row per (run_id, slice_id, ROI) in eligible
#           slices, with columns:
#             run_id, group_id, slice_id, ROI, drug,
#             base_empty, drug_empty, hik_present, hik_empty,
#             rejected
#
#   Eligibility: slices whose treatment_idx==2 row has treatment in
#   c("senktide","TTX"). SB222200 slices are excluded.
#
#   hik_present is determined at the slice level (not per-ROI): all ROIs
#   within a slice share the same treatment row set in this dataset.
#
#   Rejection criterion (bthik):
#     rejected = base_empty & drug_empty &
#                ifelse(hik_present, hik_empty, TRUE)
#   A ROI is rejected when it is empty at baseline AND at drug time AND
#   (if a hi-K depolarization test was run for this slice) its hi-K row
#   is also empty. If no hi-K test was run, the hi-K condition is treated
#   as satisfied (TRUE) — i.e., base+drug empty is sufficient.
#
#   Stops with a diagnostic message if the data violates:
#     - Any eligible slice is missing treatment_idx==1 or treatment_idx==2 rows.
#     - ROI sets differ between treatment_idx==1 and treatment_idx==2 for any slice.
#     - group_id is not constant within a (run_id, slice_id).
#   Returns an empty data frame with the correct column structure (plus a
#   warning) if there are no eligible slices.

compute_rejection_verdict <- function(df) {
  ELIGIBLE_DRUGS <- c("senktide", "TTX")
  GROUPS         <- c("DI", "OVX", "MALE", "ORX")

  is_empty   <- function(d) d$n_events == 0 & d$freq == 0
  triple_key <- function(d) paste(d$run_id, d$slice_id, d$ROI, sep = "||")
  slice_key  <- function(d) paste(d$run_id, d$slice_id,        sep = "||")

  # ---- 1. Eligibility ---------------------------------------------------
  elig <- unique(df[df$treatment_idx == 2L & df$treatment %in% ELIGIBLE_DRUGS,
                    c("run_id", "slice_id")])

  if (nrow(elig) == 0L) {
    warning("compute_rejection_verdict: no eligible slices in input")
    return(data.frame(
      run_id      = character(), group_id    = character(),
      slice_id    = character(), ROI         = character(),
      drug        = character(), base_empty  = logical(),
      drug_empty  = logical(),  hik_present = logical(),
      hik_empty   = logical(),  rejected    = logical(),
      stringsAsFactors = FALSE
    ))
  }

  elig_key <- slice_key(elig)

  # ---- 2. Subset to eligible slices ------------------------------------
  df_elig <- df[slice_key(df) %in% elig_key, ]

  # ---- 3–5. Row subsets by treatment phase -----------------------------
  idx1_rows <- df_elig[df_elig$treatment_idx == 1L, ]
  idx2_rows <- df_elig[df_elig$treatment_idx == 2L, ]
  hik_rows  <- df_elig[df_elig$treatment == "high K+", ]

  base_empty_key <- triple_key(unique(idx1_rows[is_empty(idx1_rows),
                                                c("run_id","slice_id","ROI")]))
  drug_empty_key <- triple_key(unique(idx2_rows[is_empty(idx2_rows),
                                                c("run_id","slice_id","ROI")]))
  hik_empty_key  <- triple_key(unique(hik_rows[is_empty(hik_rows),
                                               c("run_id","slice_id","ROI")]))

  # ---- 6. hi-K presence per slice --------------------------------------
  hik_present_key <- unique(slice_key(hik_rows))

  # ---- Defensive checks ------------------------------------------------
  slices_idx1 <- unique(slice_key(idx1_rows))
  slices_idx2 <- unique(slice_key(idx2_rows))

  missing_idx1 <- elig_key[!elig_key %in% slices_idx1]
  if (length(missing_idx1) > 0)
    stop(sprintf("No treatment_idx==1 rows for %d eligible slice(s): %s",
                 length(missing_idx1),
                 paste(head(missing_idx1, 5), collapse = ", ")))

  missing_idx2 <- elig_key[!elig_key %in% slices_idx2]
  if (length(missing_idx2) > 0)
    stop(sprintf("No treatment_idx==2 rows for %d eligible slice(s): %s",
                 length(missing_idx2),
                 paste(head(missing_idx2, 5), collapse = ", ")))

  roi1_by_slice <- split(idx1_rows$ROI, slice_key(idx1_rows))
  roi2_by_slice <- split(idx2_rows$ROI, slice_key(idx2_rows))
  roi_mismatch  <- vapply(names(roi2_by_slice), function(k) {
    !identical(sort(unique(roi1_by_slice[[k]])),
               sort(unique(roi2_by_slice[[k]])))
  }, logical(1L))
  if (any(roi_mismatch)) {
    bad <- names(roi_mismatch)[roi_mismatch]
    stop(sprintf("ROI sets differ between treatment_idx 1 and 2 for %d slice(s): %s",
                 sum(roi_mismatch), paste(head(bad, 5), collapse = ", ")))
  }

  grp_df     <- unique(df_elig[, c("run_id", "slice_id", "group_id")])
  grp_counts <- table(slice_key(grp_df))
  bad_grp    <- names(grp_counts)[grp_counts > 1L]
  if (length(bad_grp) > 0)
    stop(sprintf("group_id is not constant for %d slice(s): %s",
                 length(bad_grp), paste(head(bad_grp, 5), collapse = ", ")))

  # ---- 7. Universe: union of idx1 and idx2 ROIs -----------------------
  univ <- unique(rbind(
    idx1_rows[, c("run_id", "slice_id", "ROI")],
    idx2_rows[, c("run_id", "slice_id", "ROI")]
  ))

  # ---- 8. Build verdict table ------------------------------------------
  univ_triple_key <- triple_key(univ)
  univ_slice_key  <- slice_key(univ)

  drug_by_slice <- unique(idx2_rows[, c("run_id", "slice_id", "treatment")])
  drug_key      <- slice_key(drug_by_slice)

  grp_key <- slice_key(grp_df)

  base_empty  <- univ_triple_key %in% base_empty_key
  drug_empty  <- univ_triple_key %in% drug_empty_key
  hik_present <- univ_slice_key  %in% hik_present_key
  hik_empty   <- ifelse(hik_present, univ_triple_key %in% hik_empty_key, NA)
  rejected    <- base_empty & drug_empty & ifelse(hik_present, hik_empty, TRUE)

  verdict <- data.frame(
    run_id      = univ$run_id,
    group_id    = grp_df$group_id[match(univ_slice_key, grp_key)],
    slice_id    = univ$slice_id,
    ROI         = univ$ROI,
    drug        = drug_by_slice$treatment[match(univ_slice_key, drug_key)],
    base_empty  = base_empty,
    drug_empty  = drug_empty,
    hik_present = hik_present,
    hik_empty   = hik_empty,
    rejected    = rejected,
    stringsAsFactors = FALSE
  )

  # ---- 9. Sort ---------------------------------------------------------
  verdict$group_id <- factor(verdict$group_id, levels = GROUPS)
  verdict <- verdict[order(match(verdict$group_id, GROUPS),
                            verdict$slice_id, verdict$ROI), ]
  rownames(verdict) <- NULL
  verdict
}
