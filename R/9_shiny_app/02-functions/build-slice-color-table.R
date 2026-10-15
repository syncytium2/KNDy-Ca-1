build_slice_color_table <- function(csv_paths, output_path) {

  # ---- Step A: Read, normalize treatment, filter ----------------------------

  normalize_tx <- function(x) {
    dplyr::case_when(
      grepl("^ba", x, ignore.case = TRUE) ~ "baseline",
      grepl("^se", x, ignore.case = TRUE) ~ "senktide",
      grepl("^tt", x, ignore.case = TRUE) ~ "TTX",
      grepl("^sb", x, ignore.case = TRUE) ~ "SB222200",
      grepl("^hi", x, ignore.case = TRUE) ~ "high K+",
      grepl("^wa", x, ignore.case = TRUE) ~ "wash",
      TRUE ~ NA_character_
    )
  }

  dbs <- lapply(names(csv_paths), function(nm) {
    cat(sprintf("  Reading %s ...\n", nm))
    df <- read.csv(csv_paths[[nm]], stringsAsFactors = FALSE)
    df$treatment <- normalize_tx(df$treatment)
    df <- df[df$mouse_id != 81, ]
    df$.db <- nm
    df
  })
  all_data <- dplyr::bind_rows(dbs)
  cat(sprintf("Combined: %d rows across %d CSVs\n\n", nrow(all_data), length(csv_paths)))

  # ---- Step B: Data-purity checks -------------------------------------------

  errors <- character(0)

  # B1: No NA treatment after normalization
  b1_fail <- all_data[is.na(all_data$treatment), ]
  if (nrow(b1_fail) > 0) {
    errors <- c(errors, sprintf(
      "B1 FAIL: %d rows have NA treatment after normalization. DBs: %s",
      nrow(b1_fail), paste(unique(b1_fail$.db), collapse = ", ")
    ))
  } else {
    cat("B1 pass: no NA treatment values.\n")
  }

  # B2: treatment_idx == 1 must be "baseline"
  b2_fail <- all_data[all_data$treatment_idx == 1 & all_data$treatment != "baseline", ]
  if (nrow(b2_fail) > 0) {
    errors <- c(errors, sprintf(
      "B2 FAIL: %d rows have treatment_idx == 1 but treatment != 'baseline'. Slices: %s",
      nrow(b2_fail), paste(unique(b2_fail$slice_id), collapse = ", ")
    ))
  } else {
    cat("B2 pass: all treatment_idx == 1 rows are 'baseline'.\n")
  }

  # B3: treatment_idx == 2 must not be "baseline"
  b3_fail <- all_data[all_data$treatment_idx == 2 & all_data$treatment == "baseline", ]
  if (nrow(b3_fail) > 0) {
    errors <- c(errors, sprintf(
      "B3 FAIL: %d rows have treatment_idx == 2 but treatment == 'baseline'. Slices: %s",
      nrow(b3_fail), paste(unique(b3_fail$slice_id), collapse = ", ")
    ))
  } else {
    cat("B3 pass: no 'baseline' at treatment_idx == 2.\n")
  }

  # B4: Cross-CSV agreement on treatment at idx 2 per slice
  idx2 <- all_data[all_data$treatment_idx == 2, ]
  b4_check <- dplyr::summarise(
    dplyr::group_by(idx2, slice_id),
    n_tx = dplyr::n_distinct(treatment),
    vals = paste(sort(unique(treatment)), collapse = " / "),
    .groups = "drop"
  )
  b4_fail <- b4_check[b4_check$n_tx > 1, ]
  if (nrow(b4_fail) > 0) {
    errors <- c(errors, sprintf(
      "B4 FAIL: %d slice(s) have conflicting treatment at idx 2 across CSVs:\n  %s",
      nrow(b4_fail),
      paste(sprintf("  %s: %s", b4_fail$slice_id, b4_fail$vals), collapse = "\n")
    ))
  } else {
    cat("B4 pass: all slices agree on treatment at idx 2 across all CSVs.\n")
  }

  # B5: Within-CSV, each slice has at most one distinct treatment at idx 2
  b5_check <- dplyr::summarise(
    dplyr::group_by(idx2, .db, slice_id),
    n_tx = dplyr::n_distinct(treatment),
    .groups = "drop"
  )
  b5_fail <- b5_check[b5_check$n_tx > 1, ]
  if (nrow(b5_fail) > 0) {
    errors <- c(errors, sprintf(
      "B5 FAIL: %d (db, slice) pairs have multiple treatments at idx 2 within one CSV:\n  %s",
      nrow(b5_fail),
      paste(sprintf("  %s / %s", b5_fail$.db, b5_fail$slice_id), collapse = "\n")
    ))
  } else {
    cat("B5 pass: no within-CSV idx-2 conflicts.\n")
  }

  # B6: Every slice present in all CSVs
  n_expected <- length(csv_paths)
  slice_db_counts <- dplyr::summarise(
    dplyr::group_by(dplyr::distinct(all_data, slice_id, .db), slice_id),
    n_dbs = dplyr::n_distinct(.db),
    .groups = "drop"
  )
  b6_fail <- slice_db_counts[slice_db_counts$n_dbs < n_expected, ]
  if (nrow(b6_fail) > 0) {
    errors <- c(errors, sprintf(
      "B6 FAIL: %d slice(s) missing from at least one CSV:\n  %s",
      nrow(b6_fail),
      paste(sprintf("  %s (in %d/%d CSVs)", b6_fail$slice_id, b6_fail$n_dbs, n_expected),
            collapse = "\n")
    ))
  } else {
    cat("B6 pass: all slices present in all CSVs.\n")
  }

  if (length(errors) > 0) {
    stop("\nData purity checks FAILED — table not written:\n\n",
         paste(errors, collapse = "\n\n"), call. = FALSE)
  }

  cat("\nAll purity checks passed.\n\n")

  # ---- Step C: Classify slices ----------------------------------------------

  # B7: group_id invariant per slice across all CSVs (warning, not abort)
  slice_grp <- dplyr::summarise(
    dplyr::group_by(all_data, slice_id),
    n_groups  = dplyr::n_distinct(group_id),
    group_id  = paste(sort(unique(group_id)), collapse = "/"),
    .groups   = "drop"
  )
  b7_fail <- slice_grp[slice_grp$n_groups > 1, ]
  if (nrow(b7_fail) > 0) {
    cat("WARNING (B7): slices with conflicting group_id across CSVs:\n")
    print(as.data.frame(b7_fail))
    cat("\n")
  }

  # treatment at idx 2 per slice (any CSV — B4 guarantees agreement)
  tx_idx2 <- dplyr::summarise(
    dplyr::group_by(idx2, slice_id),
    treatment_at_idx2 = dplyr::first(treatment),
    .groups = "drop"
  )

  slice_info <- dplyr::left_join(
    slice_grp[, c("slice_id", "group_id")],
    tx_idx2,
    by = "slice_id"
  )
  slice_info$tier <- dplyr::case_when(
    !is.na(slice_info$treatment_at_idx2) & slice_info$treatment_at_idx2 == "TTX"      ~ "BA-TTX",
    !is.na(slice_info$treatment_at_idx2) & slice_info$treatment_at_idx2 == "senktide" ~ "BA-senktide",
    TRUE ~ "baseline-only"
  )

  # ---- Step D: Assign colors and shapes -------------------------------------

  ba_ttx_shapes  <- slice_shapes_v2[1:2]  # 16, 17 — circle, triangle
  ba_skt_shapes  <- slice_shapes_v2[1:2]  # 16, 17 — circle, triangle
  base_shapes    <- slice_shapes_v2[3:4]  # 15, 18 — square, diamond

  add_aes <- function(df, tier_shapes, group_label, tier_label) {
    if (nrow(df) == 0) return(df)
    df <- df[order(df$slice_id), ]
    ranks <- seq_len(nrow(df)) - 1L
    if (max(ranks) >= 24L) {
      warning(sprintf(
        "Group '%s' tier '%s' has %d slices (>= 24): shape palette will recycle.",
        group_label, tier_label, nrow(df)
      ))
    }
    df$color <- slice_colors_v2[(ranks %% 12L) + 1L]
    df$shape <- tier_shapes[pmin((ranks %/% 12L) + 1L, 2L)]
    df
  }

  result_list <- lapply(split(slice_info, slice_info$group_id), function(df) {
    grp  <- df$group_id[1]
    ttx  <- add_aes(df[df$tier == "BA-TTX",        ], ba_ttx_shapes, grp, "BA-TTX")
    skt  <- add_aes(df[df$tier == "BA-senktide",   ], ba_skt_shapes, grp, "BA-senktide")
    base <- add_aes(df[df$tier == "baseline-only", ], base_shapes,   grp, "baseline-only")
    rbind(ttx, skt, base)
  })
  result <- do.call(rbind, result_list)
  result <- result[order(result$group_id, result$tier, result$slice_id), ]
  rownames(result) <- NULL
  result <- result[, c("slice_id", "group_id", "treatment_at_idx2", "tier", "color", "shape")]

  # ---- Step E: Write and summarize ------------------------------------------

  write.csv(result, output_path, row.names = FALSE)

  tbl <- as.data.frame(
    tidyr::pivot_wider(
      dplyr::summarise(
        dplyr::group_by(result, group_id, tier),
        n = dplyr::n(), .groups = "drop"
      ),
      names_from  = tier,
      values_from = n,
      values_fill = 0L
    )
  )
  tbl$total <- rowSums(tbl[, setdiff(names(tbl), "group_id")])

  cat("Per-group counts:\n")
  print(tbl)
  cat(sprintf("\nTotal slices: %d\nWrote %d rows to %s\n", nrow(result), nrow(result), output_path))

  invisible(result)
}
