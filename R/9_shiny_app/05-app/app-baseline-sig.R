library(shiny)
library(here)
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB; files stay local
if (.Platform$OS.type == "windows") windowsFonts(Arial = windowsFont("Arial"))
source(here("01-scripts/01-set-up.R"))

# ---- Constants -----------------------------------------------------------

GROUPS   <- c("DI", "OVX", "MALE", "ORX")
DATA_RUN <- "ROI_MLspike_sigmaest_5"

var_specs <- list(
  frequency = list(roi_col = "freq",        ylabel = "frequency (Hz)",    lmm_key = "freq",
                   scaleYtype = "pseudolog", plot_y_min = 0,    plot_y_max = 0.7,
                   forceYticks = c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1), td_sigma = 0.003),
  amplitude = list(roi_col = "amp_median",   ylabel = "amplitude (ΔF/F0)", lmm_key = "amp",
                   scaleYtype = "log",       plot_y_min = 0.003, plot_y_max = 2,
                   forceYticks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1), td_sigma = NULL),
  width     = list(roi_col = "width_median", ylabel = "width (seconds)",   lmm_key = "width",
                   scaleYtype = "log",       plot_y_min = 0.1,  plot_y_max = 30,
                   forceYticks = c(0.3, 1, 3, 10), td_sigma = NULL)
)

effective_vs <- function(variable, run_id, y_overrides) {
  vs  <- var_specs[[variable]]
  key <- paste0(run_id, "/", variable)
  ov  <- y_overrides[[key]]
  if (!is.null(ov)) {
    vs$scaleYtype <- ov$scaleYtype
    vs$plot_y_min <- ov$y_min
    vs$plot_y_max <- ov$y_max
  }
  vs
}

contrast_choices <- list(
  "DI vs OVX — gonad effect, females"  = "DI_OVX",
  "MALE vs ORX — gonad effect, males"  = "MALE_ORX",
  "DI vs MALE — sex effect, intact"    = "DI_MALE",
  "OVX vs ORX — sex effect, GDX"       = "OVX_ORX",
  "DI vs ORX — sex + gonad, f-int/m-gdx" = "DI_ORX",
  "OVX vs MALE — sex + gonad, f-gdx/m-int" = "OVX_MALE"
)

contrast_pairs <- list(
  DI_OVX   = c("DI",   "OVX"),
  MALE_ORX = c("MALE", "ORX"),
  DI_MALE  = c("DI",   "MALE"),
  OVX_ORX  = c("OVX",  "ORX"),
  DI_ORX   = c("DI",   "ORX"),
  OVX_MALE = c("OVX",  "MALE")
)


# ---- Helpers -------------------------------------------------------------

emm_to_groupid <- function(sex, gonads) {
  dplyr::case_when(
    sex == "female" & gonads == "intact" ~ "DI",
    sex == "female" & gonads == "GDX"    ~ "OVX",
    sex == "male"   & gonads == "intact" ~ "MALE",
    sex == "male"   & gonads == "GDX"    ~ "ORX"
  )
}

cld_fn <- utils::getFromNamespace("cld.emmGrid", "emmeans")

get_cld_df <- function(emm, adjust = "sidak") {
  cld_df          <- as.data.frame(cld_fn(emm, Letters = letters, adjust = adjust))
  cld_df$group_id <- emm_to_groupid(cld_df$sex, cld_df$gonads)
  cld_df$.group   <- trimws(cld_df$.group)
  cld_df[order(match(cld_df$group_id, GROUPS)), ]
}

get_baseline_pairs <- function(emm, adjust = "sidak") {
  pw_df <- as.data.frame(pairs(emm, adjust = adjust))

  # Map each contrast to its two groups using the emmeans grid row order.
  # emmeans::pairs() generates (i,j) pairs in row-major upper-triangular order:
  # (1,2),(1,3),(1,4),(2,3),(2,4),(3,4) for n=4 cells.
  emm_cells <- as.data.frame(emm)[, c("sex", "gonads")]
  emm_cells$group_id <- emm_to_groupid(emm_cells$sex, emm_cells$gonads)
  n   <- nrow(emm_cells)
  idx <- which(upper.tri(matrix(0L, n, n)), arr.ind = TRUE)
  idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]   # row-major sort

  pw_df$group1 <- emm_cells$group_id[idx[, 1]]
  pw_df$group2 <- emm_cells$group_id[idx[, 2]]

  pw_df %>% dplyr::mutate(stars = dplyr::case_when(
    p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
    p.value < 0.05  ~ "*",   TRUE            ~ "ns"
  ))
}

# Build a list of linear-combination coefficient vectors for a chosen subset of
# the 6 named pair_ids (from contrast_pairs). Each coefficient vector is length
# n_cells (4) over the emmeans grid in its native row order, with +1 / -1 at the
# two cell positions corresponding to the pair's two group_ids.
build_pair_coefs <- function(emm, pair_ids) {
  emm_cells <- as.data.frame(emm)[, c("sex", "gonads")]
  emm_cells$group_id <- emm_to_groupid(emm_cells$sex, emm_cells$gonads)
  coefs <- lapply(pair_ids, function(pid) {
    grps <- contrast_pairs[[pid]]
    v <- numeric(nrow(emm_cells))
    v[match(grps[1], emm_cells$group_id)] <- 1
    v[match(grps[2], emm_cells$group_id)] <- -1
    v
  })
  # Use human-readable labels matching emmeans' usual "X - Y" format
  names(coefs) <- vapply(pair_ids, function(pid) {
    grps <- contrast_pairs[[pid]]
    paste(grps[1], "-", grps[2])
  }, character(1))
  coefs
}

# Compute baseline pairwise contrasts restricted to a chosen subset of pair_ids,
# with two adjustment-family modes:
#   "restricted"   -- adjust over only the kept contrasts (custom contrast list)
#   "filter_after" -- adjust over all 6, then keep only the chosen rows
# Returns a data frame with the same columns as get_baseline_pairs().
get_baseline_pairs_restricted <- function(emm, kept_pair_ids,
                                           adjust = "sidak",
                                           family_mode = "restricted") {
  if (length(kept_pair_ids) == 0) {
    # Caller is responsible for guarding against this; return empty shell with right cols.
    return(data.frame(
      contrast = character(0), estimate = numeric(0), SE = numeric(0),
      df = numeric(0), t.ratio = numeric(0), p.value = numeric(0),
      group1 = character(0), group2 = character(0), stars = character(0),
      stringsAsFactors = FALSE
    ))
  }

  if (family_mode == "restricted") {
    coefs   <- build_pair_coefs(emm, kept_pair_ids)
    ctr_df  <- as.data.frame(contrast(emm, method = coefs, adjust = adjust))
    # Map each row back to its pair_id via order in coefs (preserved by emmeans).
    ctr_df$group1 <- vapply(kept_pair_ids,
                            function(pid) contrast_pairs[[pid]][1], character(1))
    ctr_df$group2 <- vapply(kept_pair_ids,
                            function(pid) contrast_pairs[[pid]][2], character(1))
    ctr_df %>% dplyr::mutate(stars = dplyr::case_when(
      p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
      p.value < 0.05  ~ "*",   TRUE            ~ "ns"
    ))
  } else {
    # filter_after: full 6-pair adjustment, then drop unwanted rows.
    full <- get_baseline_pairs(emm, adjust = adjust)
    kept_pairs <- contrast_pairs[kept_pair_ids]
    keep_rows <- vapply(seq_len(nrow(full)), function(i) {
      any(vapply(kept_pairs, function(comp) {
        (full$group1[i] == comp[1] && full$group2[i] == comp[2]) ||
        (full$group1[i] == comp[2] && full$group2[i] == comp[1])
      }, logical(1)))
    }, logical(1))
    full[keep_rows, , drop = FALSE]
  }
}

# Compute CLD letters from a restricted contrast set. We can't use cld.emmGrid()
# directly here because it always operates on the full pairwise family of an
# emmGrid. Instead, derive letters manually from the pairwise p-values in
# `pairs_df` (output of get_baseline_pairs_restricted), treating any pair NOT in
# the restricted set as "not tested" (= not significantly different by default).
# Returns a data frame with columns: sex, gonads, group_id, .group  (matching
# get_cld_df's downstream consumers).
cld_from_restricted_pairs <- function(emm, pairs_df, alpha = 0.05) {
  emm_cells <- as.data.frame(emm)[, c("sex", "gonads")]
  emm_cells$group_id <- emm_to_groupid(emm_cells$sex, emm_cells$gonads)
  emm_cells <- emm_cells[order(match(emm_cells$group_id, GROUPS)), , drop = FALSE]
  groups <- emm_cells$group_id
  n <- length(groups)

  # Build n x n logical matrix: TRUE means "significantly different" (p < alpha
  # AND the pair was tested). Untested pairs default to FALSE = not different.
  diff_mat <- matrix(FALSE, n, n, dimnames = list(groups, groups))
  for (i in seq_len(nrow(pairs_df))) {
    g1 <- pairs_df$group1[i]; g2 <- pairs_df$group2[i]
    if (is.na(g1) || is.na(g2)) next
    if (!(g1 %in% groups) || !(g2 %in% groups)) next
    is_diff <- isTRUE(pairs_df$p.value[i] < alpha)
    diff_mat[g1, g2] <- is_diff
    diff_mat[g2, g1] <- is_diff
  }

  # Compact-letter assignment via maximal-clique enumeration of the
  # "not-significantly-different" graph. With only 4 groups, we can afford
  # exhaustive subset enumeration (16 subsets), keeping only those that are
  # cliques (all pairs non-different) AND maximal (no superset is a clique).
  # Each maximal clique gets one letter, assigned to every group it contains.
  not_diff_mat <- !diff_mat
  diag(not_diff_mat) <- TRUE  # a group is non-different from itself

  is_clique <- function(idx) {
    if (length(idx) <= 1) return(TRUE)
    all(not_diff_mat[idx, idx])
  }

  # Enumerate all non-empty subsets of group indices and keep the cliques.
  # For each integer 1..(2^n - 1), the set bits indicate which groups are in.
  all_subsets <- lapply(seq_len(2^n - 1), function(k) {
    bits <- as.integer(intToBits(k))[seq_len(n)]
    which(bits == 1L)
  })
  cliques <- Filter(is_clique, all_subsets)

  # Drop non-maximal cliques (those that are strict subsets of another clique).
  is_maximal <- function(clq, all_cs) {
    !any(vapply(all_cs, function(other) {
      length(other) > length(clq) && all(clq %in% other)
    }, logical(1)))
  }
  maximal_cliques <- Filter(function(clq) is_maximal(clq, cliques), cliques)

  # Sort maximal cliques so letter assignment is stable: by smallest member
  # index first, then by size (larger first to break ties consistently).
  maximal_cliques <- maximal_cliques[order(
    vapply(maximal_cliques, min,    integer(1)),
    -vapply(maximal_cliques, length, integer(1))
  )]

  letters_assigned <- character(n)
  names(letters_assigned) <- groups
  for (g in groups) letters_assigned[g] <- ""

  for (i in seq_along(maximal_cliques)) {
    if (i > length(letters)) break  # cap on a-z; with n=4 we never exceed
    L <- letters[i]
    for (j in maximal_cliques[[i]]) {
      letters_assigned[groups[j]] <- paste0(letters_assigned[groups[j]], L)
    }
  }

  # Any group with no letter (shouldn't happen because each group is in at
  # least the singleton clique {g}, but guard anyway).
  for (g in groups) {
    if (letters_assigned[g] == "") letters_assigned[g] <- "?"
  }

  out <- emm_cells
  out$.group <- letters_assigned[out$group_id]
  out
}

get_ba_treatment_pairs <- function(lmm, adjust = "sidak") {
  emm <- emmeans(lmm, ~ treatment | sex * gonads)
  as.data.frame(contrast(emm, method = "pairwise", adjust = adjust)) %>%
    dplyr::mutate(
      group_id = emm_to_groupid(sex, gonads),
      stars    = dplyr::case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   TRUE            ~ "ns"
      )
    )
}

p_to_label <- function(p, label_type) {
  if (label_type == "stars")
    dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns")
  else
    sprintf("p=%.3f", p)
}

format_lmm_table <- function(lmm) {
  at <- lmm$anova_table
  pv <- at[["Pr(>F)"]]
  data.frame(
    Effect    = rownames(at),
    `F value` = round(at[["F"]], 2),
    p         = formatC(pv, format = "f", digits = 3),
    ` `       = ifelse(pv < 0.001, "***",
                ifelse(pv < 0.01,  "**",
                ifelse(pv < 0.05,  "*", ""))),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
}

format_pairs_table <- function(pairs_df, type = "baseline") {
  if (type == "baseline") {
    df <- pairs_df[!is.na(pairs_df$group1), ]
    df <- df[order(match(df$group1, GROUPS), match(df$group2, GROUPS)), ]
    data.frame(
      `Group 1` = df$group1,
      `Group 2` = df$group2,
      Estimate  = round(df$estimate, 4),
      SE        = round(df$SE,       4),
      `t ratio` = round(df$t.ratio,  3),
      p         = formatC(df$p.value, format = "f", digits = 4),
      ` `       = df$stars,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else {
    df <- pairs_df[order(match(pairs_df$group_id, GROUPS)), ]
    data.frame(
      Group     = as.character(df$group_id),
      Contrast  = df$contrast,
      Estimate  = round(df$estimate, 4),
      SE        = round(df$SE,       4),
      `t ratio` = round(df$t.ratio,  3),
      p         = formatC(df$p.value, format = "f", digits = 4),
      ` `       = df$stars,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}

visual_y <- function(fracs, y_min, y_max, scale_type) {
  switch(scale_type,
    log = {
      lmin <- log10(max(y_min, 1e-10))
      lmax <- log10(y_max)
      10^(lmin + fracs * (lmax - lmin))
    },
    pseudolog  = y_min + fracs * (y_max - y_min),
    squareRoot = (fracs * sqrt(y_max))^2,
    normal     = y_min + fracs * (y_max - y_min)
  )
}

# Extend the y_max upward so that the original y_max maps to NPC (1 - reserved).
# This creates a blank zone at the top for bracket annotations.
extend_y_for_brackets <- function(y_min, y_max, reserved, scale_type) {
  if (reserved <= 0) return(y_max)
  switch(scale_type,
    log = {
      lmin <- log10(max(y_min, 1e-10))
      lmax <- log10(y_max)
      10^(lmin + (lmax - lmin) / (1 - reserved))
    },
    squareRoot = {
      smin <- sqrt(max(y_min, 0))
      smax <- sqrt(y_max)
      (smin + (smax - smin) / (1 - reserved))^2
    },
    y_min + (y_max - y_min) / (1 - reserved)   # linear / pseudolog / normal
  )
}

# ---- Bracket rendering helpers -------------------------------------------

# Build a significance bracket as a grid grob in panel-npc coordinates.
# x1/x2/y/tick_h are all fractions [0,1] of the panel area; this bypasses
# ggplot's data-coordinate system entirely.
make_bracket_grob <- function(x1, x2, y, label, tick_h = 0.02, fontsize = 9, lwd = 1) {
  grid::grobTree(
    grid::linesGrob(x = c(x1, x2), y = c(y, y),         gp = grid::gpar(lwd = lwd)),
    grid::linesGrob(x = c(x1, x1), y = c(y - tick_h, y), gp = grid::gpar(lwd = lwd)),
    grid::linesGrob(x = c(x2, x2), y = c(y - tick_h, y), gp = grid::gpar(lwd = lwd)),
    grid::textGrob(label, x = (x1 + x2) / 2, y = y + 0.018,
                   gp = grid::gpar(fontsize = fontsize))
  )
}

# Map a group name to its panel-npc x position.
# Assumes GROUPS order → integer positions 1..n with ggplot2 discrete expansion ~±0.6.
group_to_npc_x <- function(g) {
  pos   <- match(g, GROUPS)
  x_min <- 0.4; x_max <- length(GROUPS) + 0.6
  (pos - x_min) / (x_max - x_min)
}

# Abbreviate before/after x-axis treatment labels.
#   baseline -> "b"
#   senktide -> "s"
#   TTX      -> "TTX"   (kept as-is)
#   anything else -> first three letters (case-preserved)
# Vectorized over input.
abbrev_treatment_label <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    sl <- tolower(s)
    if (sl == "baseline")        "b"
    else if (sl == "senktide")   "s"
    else if (sl == "ttx")        "TTX"
    else                         substr(s, 1, 3)
  }, character(1), USE.NAMES = FALSE)
}

# Apply app-wide theme tweaks to a freshly-built ggplot. Currently:
#   - tightens the gap between the y-axis title and the tick labels
#     (axis.title.y margin's right side)
#   - for BA plots, abbreviates x-axis treatment labels via the labeller above
# Returns a ggplot (still composable with + further layers).
apply_app_theme_and_labels <- function(p, plot_type) {
  p <- p + ggplot2::theme(
    # Slight negative right margin pulls the y-axis title closer to the tick
    # labels. Now that the label string itself is shorter (scale-type suffix
    # stripped at the function level), only a small pull is needed. If the
    # title clips into the ticks, raise toward 0; for more space, raise above 0.
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = -1))
  )
  if (identical(plot_type, "ba")) {
    # before_after_v6 may already set a discrete x scale; adding ours will
    # replace it. ggplot emits a one-off "Scale for x is already present"
    # message in that case — harmless, leave it.
    p <- p + ggplot2::scale_x_discrete(labels = abbrev_treatment_label)
  }
  p
}

# Add baseline significance brackets to a ggplot using panel-npc coordinates.
# Stacks brackets downward from y_top; step size y_step per bracket.
#
# DEPRECATED: superseded by add_baseline_brackets_gtable() below. The
# annotation_custom-based approach here interacts poorly with ggplot's data
# coordinate system when y is log/pseudolog/sqrt transformed, causing brackets
# to drop or render in the wrong place. Kept here for reference; not called
# from the live app.
add_baseline_brackets_npc <- function(p, pairs_df, mode, contrast_sel,
                                       p_label_type, alpha,
                                       y_top = 0.92, y_step = 0.07) {
  lookup_pair <- function(comp) {
    dplyr::filter(pairs_df,
      (group1 == comp[1] & group2 == comp[2]) |
      (group1 == comp[2] & group2 == comp[1])
    )
  }

  if (mode == "all") {
    brk_list <- Filter(function(comp) {
      row <- lookup_pair(comp)
      nrow(row) > 0 && row$p.value[1] <= alpha
    }, contrast_pairs)
  } else {
    if (!nzchar(contrast_sel)) return(p)
    comp <- contrast_pairs[[contrast_sel]]
    row  <- lookup_pair(comp)
    brk_list <- if (nrow(row) > 0 && row$p.value[1] <= alpha)
      setNames(list(comp), contrast_sel) else list()
  }

  if (length(brk_list) == 0) return(p)

  n      <- length(brk_list)
  y_npcs <- seq(y_top, y_top - (n - 1) * y_step, length.out = n)

  for (i in seq_along(brk_list)) {
    comp  <- brk_list[[i]]
    row   <- lookup_pair(comp)
    label <- p_to_label(row$p.value[1], p_label_type)
    grob  <- make_bracket_grob(group_to_npc_x(comp[1]), group_to_npc_x(comp[2]),
                                y_npcs[i], label)
    p <- p + ggplot2::annotation_custom(grob, xmin = -Inf, xmax = Inf,
                                         ymin = -Inf, ymax = Inf)
  }
  p
}

# Map treatment position to NPC x for the 2-group BA discrete axis.
# Default ggplot2 discrete expansion is add=0.5 on each side.
ba_npc_x <- function(pos) {
  (pos - 0.5) / 2.0   # pos=1 → 0.25, pos=2 → 0.75
}

# Add before-after significance brackets to a faceted ggplot via gtable manipulation.
# Returns a gtable (not a ggplot) with bracket grobs added to the appropriate facet panels.
# annot_df: data frame with columns group_id, p.value (already filtered by alpha)
# trt: treatment name string (the "after" condition)
# p_label_type: "stars" or "p.value"
# y_npc: NPC y position for bracket (default 0.88)
add_ba_brackets_gtable <- function(p, annot_df, trt, p_label_type, y_npc = 0.88) {
  if (nrow(annot_df) == 0) return(ggplotGrob(p))

  gt <- ggplotGrob(p)

  # Find all panel cells in the gtable layout
  panel_idx    <- grep("^panel", gt$layout$name)
  panel_layout <- gt$layout[panel_idx, ]
  # Sort by left column index (l) to get left-to-right order = GROUPS order
  panel_layout <- panel_layout[order(panel_layout$l), ]
  # The groups in left-to-right order match GROUPS = c("DI","OVX","MALE","ORX")
  group_order  <- GROUPS

  x1 <- ba_npc_x(1)   # baseline NPC x ≈ 0.25
  x2 <- ba_npc_x(2)   # treatment NPC x ≈ 0.75

  for (i in seq_len(nrow(annot_df))) {
    row_g   <- annot_df[i, , drop = FALSE]
    grp     <- row_g$group_id
    label   <- p_to_label(row_g$p.value, p_label_type)
    grob    <- make_bracket_grob(x1, x2, y_npc, label)

    grp_idx <- match(grp, group_order)
    if (is.na(grp_idx) || grp_idx > nrow(panel_layout)) next

    cell <- panel_layout[grp_idx, ]
    gt   <- gtable::gtable_add_grob(
      gt, grob,
      t    = cell$t, l = cell$l, b = cell$b, r = cell$r,
      name = paste0("bracket_", grp),
      clip = "on"
    )
  }
  gt
}

# Add baseline significance brackets to a single-panel ggplot via gtable
# manipulation. Returns a gtable (not a ggplot) with bracket grobs added to
# the single panel cell.
#
# This is the post-render counterpart to the older add_baseline_brackets_npc()
# (which used annotation_custom). The annotation_custom approach interacts
# poorly with data-coordinate y-axis transforms (log / pseudolog / sqrt) and
# with the reserved bracket zone above the data — the gtable approach bypasses
# the data scale entirely, mirroring how BA mode places its brackets.
#
# Args mirror add_baseline_brackets_npc().
add_baseline_brackets_gtable <- function(p, pairs_df, mode, contrast_sel,
                                          p_label_type, alpha,
                                          y_top = 0.92, y_step = 0.07) {
  lookup_pair <- function(comp) {
    dplyr::filter(pairs_df,
      (group1 == comp[1] & group2 == comp[2]) |
      (group1 == comp[2] & group2 == comp[1])
    )
  }

  # Decide which contrasts get brackets (same logic as the old function).
  if (mode == "all") {
    brk_list <- Filter(function(comp) {
      row <- lookup_pair(comp)
      nrow(row) > 0 && row$p.value[1] <= alpha
    }, contrast_pairs)
  } else {
    if (!nzchar(contrast_sel)) return(ggplotGrob(p))
    comp <- contrast_pairs[[contrast_sel]]
    row  <- lookup_pair(comp)
    brk_list <- if (nrow(row) > 0 && row$p.value[1] <= alpha)
      setNames(list(comp), contrast_sel) else list()
  }

  # Convert to gtable up-front so we always return the same type, regardless
  # of whether brackets were drawn. Downstream code (renderers, summary PDF)
  # already handles both ggplot and gtable inputs.
  gt <- ggplotGrob(p)
  if (length(brk_list) == 0) return(gt)

  # Find the single panel cell. plot_baseline produces a non-faceted plot,
  # so there's exactly one panel.
  panel_idx    <- grep("^panel", gt$layout$name)
  panel_layout <- gt$layout[panel_idx, , drop = FALSE]
  if (nrow(panel_layout) == 0) return(gt)
  cell <- panel_layout[1, , drop = FALSE]

  # Stack brackets downward from y_top with step size y_step. Y positions are
  # in the panel's NPC coordinates, so they're independent of the y-axis scale
  # and unaffected by the y_max extension done upstream.
  n      <- length(brk_list)
  y_npcs <- seq(y_top, y_top - (n - 1) * y_step, length.out = n)

  for (i in seq_along(brk_list)) {
    comp  <- brk_list[[i]]
    row   <- lookup_pair(comp)
    label <- p_to_label(row$p.value[1], p_label_type)
    grob  <- make_bracket_grob(
      group_to_npc_x(comp[1]), group_to_npc_x(comp[2]),
      y_npcs[i], label
    )
    gt <- gtable::gtable_add_grob(
      gt, grob,
      t    = cell$t, l = cell$l, b = cell$b, r = cell$r,
      name = paste0("baseline_bracket_", names(brk_list)[i]),
      clip = "off"   # let bracket text breathe slightly above the panel if needed
    )
  }
  gt
}

# Write a ggplot object to a file at the given dimensions using the named device.
render_to_file <- function(path, fmt, width, height, dpi, plot_obj) {
  switch(fmt,
    png  = grDevices::png( path, width = width, height = height, units = "in", res = dpi),
    pdf  = if (capabilities("cairo"))
             grDevices::cairo_pdf(path, width = width, height = height)
           else
             grDevices::pdf(path, width = width, height = height, useDingbats = FALSE),
    svg  = svglite::svglite(path, width = width, height = height),
    tiff = grDevices::tiff(path, width = width, height = height,
                           units = "in", res = dpi, compression = "lzw"),
    eps  = if (capabilities("cairo"))
             grDevices::cairo_ps(path, width = width, height = height, onefile = FALSE)
           else
             grDevices::postscript(path, width = width, height = height,
                                   onefile = FALSE, paper = "special", horizontal = FALSE)
  )
  if (inherits(plot_obj, "gtable")) grid::grid.draw(plot_obj) else print(plot_obj)
  grDevices::dev.off()
}

# Draw a titled table inside a grid viewport.
# x, y_bottom, width, height are all in inches (page coordinates, origin = bottom-left).
draw_titled_table <- function(df, caption, x, y_bottom, width, height, ttheme) {
  TITLE_H_NPC <- 0.18   # fraction of viewport height reserved for title
  # Push outer viewport
  grid::pushViewport(grid::viewport(
    x = x, y = y_bottom, width = width, height = height,
    just = c("left", "bottom"), default.units = "inches"
  ))
  # Draw caption at top of viewport
  grid::grid.text(caption,
    x = 0.02, y = 1 - 0.02,
    just = c("left", "top"),
    default.units = "npc",
    gp = grid::gpar(fontsize = 9, fontface = "bold")
  )
  # Table in sub-viewport below title, left-aligned
  grid::pushViewport(grid::viewport(
    x = 0, y = 1 - TITLE_H_NPC - 0.02,
    width = 1, height = 1 - TITLE_H_NPC - 0.04,
    just = c("left", "top"), default.units = "npc"
  ))
  tbl <- gridExtra::tableGrob(df, rows = NULL, theme = ttheme)
  grid::grid.draw(tbl)
  grid::popViewport(2)
}

# Build and write a summary PDF: plot on top, LMM/CLD tables below, pairwise at bottom.
# Fixed page size: 8.5" x 11" portrait with 0.5" margins.
# In two_pages mode: page 1 = plot only, page 2 = tables only.
make_summary_page <- function(file, plot_obj, lmm_df, cld_df, pairs_df,
                               fig_w, fig_h, title = NULL, two_pages = FALSE,
                               warnings = NULL) {
  # --- geometry constants ---
  PAGE_W     <- 8.5
  PAGE_H     <- 11.0
  M          <- 0.5          # margin
  USE_W      <- PAGE_W - 2 * M   # 7.5"
  GAP_PLOT   <- 0.25         # gap between plot bottom and top of table zone
  TITLE_H    <- 0.28         # height reserved per table section title
  GAP_TBLS   <- 0.12         # gap between LMM/CLD row and pairwise row
  BASE_ROW_H <- 0.215        # row height at base_size = 8

  plot_grob <- if (inherits(plot_obj, "gtable")) plot_obj else ggplot2::ggplotGrob(plot_obj)

  # Row counts (header counts as a row)
  n_top  <- max(nrow(lmm_df) + 1L, nrow(cld_df) + 1L)
  n_pair <- nrow(pairs_df) + 1L

  # Minimum height needed for tables at base scale
  needed <- n_top  * BASE_ROW_H + TITLE_H +
            GAP_TBLS +
            n_pair * BASE_ROW_H + TITLE_H

  if (capabilities("cairo"))
    grDevices::cairo_pdf(file, width = PAGE_W, height = PAGE_H, onefile = TRUE)
  else
    grDevices::pdf(file, width = PAGE_W, height = PAGE_H, onefile = TRUE, useDingbats = FALSE)

  # Helper: draw tables flowing top-down from tbl_start (inches from page bottom)
  draw_tables <- function(tbl_start) {
    avail     <- tbl_start - M         # space between table zone top and bottom margin
    scale     <- min(1.0, avail / needed)
    row_h     <- BASE_ROW_H * scale
    base_size <- max(5L, round(8 * scale))
    ttheme    <- gridExtra::ttheme_default(base_size = base_size,
                   core    = list(fg_params = list(cex = scale)),
                   colhead = list(fg_params = list(cex = scale)))

    toprow_h <- n_top  * row_h + TITLE_H
    pair_h   <- n_pair * row_h + TITLE_H

    toprow_top <- tbl_start              # LMM/CLD top
    toprow_bot <- toprow_top - toprow_h  # LMM/CLD bottom
    pair_top   <- toprow_bot - GAP_TBLS  # Pairwise top
    pair_bot   <- pair_top   - pair_h    # Pairwise bottom

    # LMM ANOVA — left half of top row
    draw_titled_table(
      df       = lmm_df,
      caption  = "LMM ANOVA",
      x        = M,
      y_bottom = toprow_bot,
      width    = 3.55,
      height   = toprow_h,
      ttheme   = ttheme
    )

    # CLD Letters — right half of top row
    draw_titled_table(
      df       = cld_df,
      caption  = "CLD Letters",
      x        = M + 3.55 + 0.4,
      y_bottom = toprow_bot,
      width    = 3.55,
      height   = toprow_h,
      ttheme   = ttheme
    )

    # Pairwise comparisons — full width, bottom row
    draw_titled_table(
      df       = pairs_df,
      caption  = "Pairwise Comparisons",
      x        = M,
      y_bottom = pair_bot,
      width    = USE_W,
      height   = pair_h,
      ttheme   = ttheme
    )

    if (!is.null(warnings) && length(warnings) > 0) {
      warn_txt <- paste("Model notes:", paste(warnings, collapse = "; "))
      warn_txt <- paste(strwrap(warn_txt, width = 110), collapse = "\n")
      grid::pushViewport(grid::viewport(
        x = M, y = 0.65, width = USE_W, height = 0.35,
        just = c("left", "bottom"), default.units = "inches"
      ))
      grid::grid.text(warn_txt, x = 0, y = 1, just = c("left", "top"),
        default.units = "npc",
        gp = grid::gpar(fontsize = 7.5, col = "#7a6000", fontface = "italic"))
      grid::popViewport()
    }

    # Methods footnote — drawn at very bottom of page regardless of table positions
    methods_txt <- paste(strwrap(paste0(
      "Statistics: Linear mixed model with random effects for brain slice and animal ",
      "(Kenward-Roger df approximation). ",
      "Response variables are transformed before fitting (sqrt for frequency; ",
      "log10 for amplitude and width). ",
      "Post-hoc pairwise comparisons with Sidak correction. ",
      "CLD letters: groups sharing a letter are not significantly different."
    ), width = 115), collapse = "\n")
    grid::pushViewport(grid::viewport(
      x = M, y = 0.08, width = USE_W, height = 0.55,
      just = c("left", "bottom"), default.units = "inches"
    ))
    grid::grid.text(
      methods_txt,
      x = 0, y = 1,
      just = c("left", "top"),
      default.units = "npc",
      gp = grid::gpar(fontsize = 8, col = "#888888")
    )
    grid::popViewport()
  }

  # Helper: draw plot top-anchored at plot_top_y (inches from page bottom)
  draw_plot <- function(pw, ph, plot_top_y) {
    plot_x <- M + (USE_W - pw) / 2   # centered horizontally
    grid::pushViewport(grid::viewport(
      x = plot_x, y = plot_top_y, width = pw, height = ph,
      just = c("left", "top"), default.units = "inches"
    ))
    if (inherits(plot_grob, "gtable")) grid::grid.draw(plot_grob) else print(plot_grob)
    grid::popViewport()
  }

  if (two_pages) {
    # ---- Page 1: plot only, top-anchored, exact dimensions -----------------
    pw         <- min(fig_w, USE_W)
    ph         <- fig_h
    plot_top_y <- PAGE_H - M

    grid::grid.newpage()
    if (!is.null(title) && nzchar(title)) {
      grid::grid.text(title,
        x = 0.5, y = (PAGE_H - M / 2) / PAGE_H,
        just = c("center", "center"),
        default.units = "npc",
        gp = grid::gpar(fontsize = 11, fontface = "bold")
      )
      plot_top_y <- plot_top_y - 0.3
    }
    draw_plot(pw, ph, plot_top_y)

    # ---- Page 2: tables only, top-down from top of usable area ---------------
    grid::grid.newpage()
    if (!is.null(title) && nzchar(title)) {
      grid::grid.text(title,
        x = 0.5, y = (PAGE_H - M / 2) / PAGE_H,
        just = c("center", "center"),
        default.units = "npc",
        gp = grid::gpar(fontsize = 11, fontface = "bold")
      )
    }
    draw_tables(tbl_start = PAGE_H - M)   # top of usable area on page 2

  } else {
    # ---- One page: plot top-anchored, tables flow down from plot bottom -----
    pw          <- min(fig_w, USE_W)
    ph          <- fig_h                        # exact user height, never scaled
    plot_top_y  <- PAGE_H - M                  # 10.5" — flush to top margin
    plot_bot_y  <- plot_top_y - ph

    grid::grid.newpage()

    if (!is.null(title) && nzchar(title)) {
      grid::grid.text(title,
        x = 0.5, y = (PAGE_H - M / 2) / PAGE_H,
        just = c("center", "center"),
        default.units = "npc",
        gp = grid::gpar(fontsize = 11, fontface = "bold")
      )
      plot_top_y <- plot_top_y - 0.3
      plot_bot_y <- plot_top_y - ph
    }

    draw_plot(pw, ph, plot_top_y)
    draw_tables(tbl_start = plot_bot_y - GAP_PLOT)
  }

  grDevices::dev.off()
  invisible(NULL)
}

sig_contrasts_for <- function(emm, alpha = 0.05) {
  pairs_df <- get_baseline_pairs(emm)
  names(Filter(function(comp) {
    row <- dplyr::filter(pairs_df,
      (group1 == comp[1] & group2 == comp[2]) |
      (group1 == comp[2] & group2 == comp[1])
    )
    nrow(row) > 0 && row$p.value[1] <= alpha
  }, contrast_pairs))
}

# ---- Export naming helpers -----------------------------------------------

extract_dataset <- function(filename) {
  fn <- tolower(tools::file_path_sans_ext(basename(filename)))
  dplyr::case_when(
    grepl("mlspike", fn) ~ "MLspike",
    grepl("custard", fn) ~ "custard",
    grepl("slow",    fn) ~ "slow",
    grepl("fast",    fn) ~ "fast",
    grepl("cms",     fn) ~ "CMS",
    TRUE ~ sub("[_-].*", "", fn)
  )
}

abbreviate_run <- function(run_id) {
  r <- sub("^ROI_", "", run_id)
  r <- gsub("(?i)mlspike_|custard_|slow_|fast_|cms_", "", r, perl = TRUE)
  r <- sub("(?i)sigmaest", "sig", r, perl = TRUE)
  r <- sub("(?i)sigma",    "sig", r, perl = TRUE)
  r <- gsub("_+", "_", r)
  r <- gsub("_([0-9])", "\\1", r)
  r <- gsub("^_|_$", "", r)
  tolower(r)
}

smart_export_name <- function(csv_filename, run_id, variable, plot_type, treatment = NULL) {
  ds  <- if (!is.null(csv_filename)) extract_dataset(csv_filename) else "data"
  run <- if (!is.null(run_id) && nzchar(run_id)) abbreviate_run(run_id) else ""
  var <- switch(variable,
    frequency = "freq", amplitude = "amp", width = "width", variable)
  pt  <- if (identical(plot_type, "ba") && !is.null(treatment))
           paste0("ba_", treatment) else plot_type
  paste(Filter(nzchar, c(ds, run, var, pt)), collapse = "_")
}

# ---- Data prep -----------------------------------------------------------

load_raw <- function(path) {
  df <- read.csv(path, head = TRUE)
  df <- df[df$mouse_id != 81, ]
  df %>% dplyr::mutate(treatment = dplyr::case_when(
    grepl("^ba", treatment, ignore.case = TRUE) ~ "baseline",
    grepl("^se", treatment, ignore.case = TRUE) ~ "senktide",
    grepl("^tt", treatment, ignore.case = TRUE) ~ "TTX",
    grepl("^sb", treatment, ignore.case = TRUE) ~ "SB222200",
    grepl("^hi", treatment, ignore.case = TRUE) ~ "high K+",
    grepl("^wa", treatment, ignore.case = TRUE) ~ "wash",
    TRUE ~ NA_character_
  ))
}

prep_baseline <- function(df_raw, run_id = DATA_RUN) {
  df <- df_raw[df_raw$run_id == run_id & round(df_raw$duration) >= 12 &
               df_raw$treatment == "baseline", ]
  df <- extractSexGonads(df)
  df$group_id <- factor(df$group_id, levels = GROUPS)
  df$slice_id <- factor(df$slice_id)
  df$sex      <- factor(df$sex,    levels = c("female", "male"))
  df$gonads   <- factor(df$gonads, levels = c("intact", "GDX"))
  df %>% dplyr::group_by(group_id) %>%
    dplyr::mutate(local_slice = as.factor(as.numeric(as.factor(slice_idx)))) %>%
    dplyr::ungroup()
}

prep_before_after <- function(df_raw, treatment_name, run_id = DATA_RUN) {
  df   <- df_raw[df_raw$run_id == run_id, ]
  ROIs <- df[round(df$duration) >= 12 & df$treatment_idx <= 2, ]
  treated <- unique(ROIs$slice_id[ROIs$treatment == treatment_name & ROIs$treatment_idx == 2])
  ROIs <- ROIs[ROIs$slice_id %in% treated, ]
  ROIs <- extractSexGonads(ROIs)
  ROIs$treatment <- factor(ROIs$treatment, levels = c("baseline", treatment_name))
  ROIs <- ROIs[!is.na(ROIs$treatment), ]
  ROIs$group_id  <- factor(ROIs$group_id,  levels = GROUPS)
  ROIs$slice_id  <- factor(ROIs$slice_id)
  ROIs$sex       <- factor(ROIs$sex,    levels = c("female", "male"))
  ROIs$gonads    <- factor(ROIs$gonads, levels = c("intact", "GDX"))
  ROIs %>% dplyr::group_by(group_id) %>%
    dplyr::mutate(local_slice = as.factor(as.numeric(as.factor(slice_idx)))) %>%
    dplyr::ungroup()
}

make_slice_summary <- function(df, col) {
  df %>%
    dplyr::group_by(group_id, slice_id, local_slice, treatment) %>%
    dplyr::summarize(
      s_mean   = mean(  .data[[col]], na.rm = TRUE),
      s_median = median(.data[[col]], na.rm = TRUE),
      .groups  = "drop"
    )
}

make_group_summary <- function(slice_df, stat = "mean_se") {
  slice_df %>%
    dplyr::group_by(group_id, treatment) %>%
    dplyr::summarize(
      g_mean   = mean(  s_mean,   na.rm = TRUE),
      g_se     = sd(    s_mean,   na.rm = TRUE) / sqrt(dplyr::n()),
      g_median = median(s_median, na.rm = TRUE),
      g_q25    = quantile(s_median, 0.25, na.rm = TRUE),
      g_q75    = quantile(s_median, 0.75, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      g_ymin = if (stat == "median_iqr") g_q25 else g_mean - g_se,
      g_ymax = if (stat == "median_iqr") g_q75 else g_mean + g_se
    )
}

capture_msgs <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(
    expr,
    warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) { msgs <<- c(msgs, trimws(conditionMessage(m))); invokeRestart("muffleMessage") }
  )
  msgs
}

translate_warnings <- function(msgs) {
  if (length(msgs) == 0) return(character(0))
  out <- character(0)
  for (m in msgs) {
    if (grepl("contr.sum", m, fixed = TRUE)) {
      next
    } else if (grepl("isSingular|boundary.*singular|singular.*boundary", m)) {
      out <- c(out, "A random-effect variance was estimated as zero — the model may have more random effects than the data can support. Fixed-effect estimates are still valid but interpret random effects cautiously.")
    } else if (grepl("misleading due to involvement in interactions", m)) {
      out <- c(out, "Marginal group means are averaged over an interaction term. When the interaction is large, the marginal means may not reflect the pattern in any single subgroup.")
    } else if (grepl("tukey.*changed to.*sidak|sidak.*instead of.*tukey", m, ignore.case = TRUE)) {
      # Stale: app now defaults to Sidak everywhere, so this warning is
      # uninformative if it ever leaks through. Silently drop.
      next
    } else if (grepl("failed to converge|did not converge", m, ignore.case = TRUE)) {
      out <- c(out, "Optimizer did not converge — results may be unreliable. Consider simplifying the random-effects structure.")
    } else if (grepl("D.f. method changed|df.*method.*changed", m, ignore.case = TRUE)) {
      next
    } else if (nzchar(trimws(m))) {
      out <- c(out, m)
    }
  }
  unique(out)
}

fit_baseline_lmms <- function(df) {
  set_sum_contrasts()
  list(
    freq  = mixed(sqrt(freq) ~ sex * gonads + (1|slice_id) + (1|mouse_id),
                  data = df, method = "KR"),
    amp   = mixed(log10(amp_median) ~ sex * gonads + (1|slice_id) + (1|mouse_id),
                  data = dplyr::filter(df, !is.na(amp_median)), method = "KR"),
    width = mixed(log10(width_median) ~ sex * gonads + (1|slice_id) + (1|mouse_id),
                  data = dplyr::filter(df, !is.na(width_median)), method = "KR")
  )
}

fit_ba_lmms <- function(df) {
  set_sum_contrasts()
  list(
    freq  = mixed(sqrt(freq) ~ sex * gonads * treatment + (1|slice_id) + (1|mouse_id),
                  data = df, method = "KR"),
    amp   = mixed(log10(amp_median) ~ sex * gonads * treatment + (1|slice_id) + (1|mouse_id),
                  data = dplyr::filter(df, !is.na(amp_median)), method = "KR"),
    width = mixed(log10(width_median) ~ sex * gonads * treatment + (1|slice_id) + (1|mouse_id),
                  data = dplyr::filter(df, !is.na(width_median)), method = "KR")
  )
}

# ---- UI ------------------------------------------------------------------

ui <- fluidPage(
  tags$style(HTML("
    .shiny-image-output { display: block; min-height: 50px; }
    .shiny-image-output img { max-width: 100%; height: auto; display: block; }
    details > summary { user-select: none; }
    details > summary::-webkit-details-marker { font-size: 10px; }

    /* Prevent browser from squeezing below minimum comfortable width */
    /* System font stack: crisp on every OS, no web-font latency */
    html, body {
      min-width: 0;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
                   'Helvetica Neue', Arial, sans-serif !important;
    }

    /* Reduce gutters: gains ~1 inch of usable width across the page */
    .container-fluid { padding-left: 6px !important; padding-right: 6px !important; }
    .row             { margin-left: -6px !important; margin-right: -6px !important; }
    .container-fluid > .row { margin-top: 10px !important; }

    /* Sidebar: fixed 200px width; main panel fills the rest */
    .col-sm-2 {
      flex: 0 0 200px !important;
      max-width: 200px !important;
      width: 200px !important;
      padding-left: 6px !important; padding-right: 6px !important;
    }
    .col-sm-10 {
      flex: 0 0 calc(100% - 200px) !important;
      max-width: calc(100% - 200px) !important;
      width: calc(100% - 200px) !important;
      padding-left: 14px !important; padding-right: 6px !important;
    }

    /* Sidebar chrome */
    .col-sm-2 {
      background: #f7f8fa !important;
      border-right: 1px solid #dee2e6 !important;
      min-height: 100vh;
    }

    /* Section summary headers */
    details > summary {
      padding: 5px 4px 5px 8px;
      border-left: 3px solid #2a7ab8;
      background: transparent;
      border-radius: 2px;
      transition: background 0.15s;
      font-size: 12.5px !important;
      font-weight: 600 !important;
      color: #2a5f82 !important;
    }
    details > summary:hover {
      background: #e8f4fd;
      color: #1a5a8a !important;
    }
    details[open] > summary {
      border-left-color: #1a5a8a;
      color: #1a5a8a !important;
    }

    /* Section content indentation */
    details > div {
      padding-left: 4px;
      padding-top: 4px;
    }

    /* Shiny input labels */
    .control-label, label {
      font-size: 12px !important;
      color: #444 !important;
      margin-bottom: 2px !important;
    }

    /* Radio and checkbox font */
    .radio label, .checkbox label {
      font-size: 12px !important;
    }

    /* Compact radio buttons */
    .radio { margin-top: 2px !important; margin-bottom: 2px !important; }

    /* Format download buttons — outlined pill style */
    .btn-default {
      border: 1px solid #2a7ab8 !important;
      color: #2a7ab8 !important;
      background: white !important;
      border-radius: 4px !important;
      font-size: 11.5px !important;
      transition: background 0.12s, color 0.12s;
    }
    .btn-default:hover {
      background: #e8f4fd !important;
      color: #1a5a8a !important;
    }

    /* Summary PDF button — filled primary */
    #dl_summary {
      background: #2a7ab8 !important;
      color: white !important;
      border: 1px solid #1a5a8a !important;
      border-radius: 4px !important;
      font-size: 11.5px !important;
      transition: background 0.12s;
    }
    #dl_summary:hover {
      background: #1a5a8a !important;
    }

    /* Full-width top bar */
    #top-bar {
      display: flex;
      align-items: center;
      gap: 14px;
      width: 100%;
      background: #f7f8fa;
      border-bottom: 1px solid #dee2e6;
      padding: 10px 15px;
      box-sizing: border-box;
    }
    /* Load CSV button — filled blue, consistent with app primary action style */
    #top-bar .btn {
      background: #2a7ab8 !important;
      color: white !important;
      border: 1px solid #1a5a8a !important;
      border-radius: 4px !important;
      font-size: 12px !important;
      padding: 5px 12px !important;
      flex-shrink: 0;
      white-space: nowrap;
      transition: background 0.12s;
    }
    #top-bar .btn:hover {
      background: #1a5a8a !important;
    }
    #top-bar-filename {
      flex: 1 1 0;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
      font-size: 13px;
      font-weight: 600;
      color: #2a7ab8;
      letter-spacing: 0.2px;
      min-width: 0;
    }
    #app_title:empty::after {
      content: 'No file loaded';
      color: #aaa;
      font-weight: 400;
      font-style: italic;
    }
    #top-bar-run-label {
      font-size: 12px;
      color: #555;
      white-space: nowrap;
      flex-shrink: 0;
    }
    #top-bar-run-wrapper {
      min-width: 400px;
      max-width: 520px;
      flex-shrink: 0;
    }
    #top-bar-run-wrapper .form-group {
      margin-bottom: 0 !important;
    }
    #top-bar-run-wrapper label {
      display: none !important;
    }
    #top-bar-run-wrapper select {
      width: 100% !important;
      font-size: 12px !important;
    }
    #top-bar-run-abbrev {
      font-size: 11px;
      color: #888;
      margin-top: 2px;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }

    /* Table base: consistent font, collapsed borders */
    .shiny-html-output table,
    .shiny-table-output table {
      font-size: 12px !important;
      border-collapse: collapse !important;
      width: 100% !important;
    }

    /* Table header cells: bold, subtle background, border-bottom anchor */
    .shiny-html-output table th,
    .shiny-table-output table th {
      font-weight: 600 !important;
      font-size: 11.5px !important;
      color: #333 !important;
      background-color: #f0f4f8 !important;
      border-bottom: 2px solid #c6d8ea !important;
      padding: 5px 8px !important;
      white-space: nowrap;
    }

    /* Table data cells: readable padding */
    .shiny-html-output table td,
    .shiny-table-output table td {
      padding: 4px 8px !important;
      vertical-align: middle !important;
      border-bottom: 1px solid #e8ecf0 !important;
    }

    /* Main panel table captions */
    table caption {
      font-weight: 700 !important;
      font-size: 12px !important;
      color: #1a5a8a !important;
      text-align: left !important;
      padding: 0 0 5px 0 !important;
      letter-spacing: 0.3px !important;
      text-transform: uppercase !important;
    }

    /* Striped table rows */
    .table-striped > tbody > tr:nth-child(odd) > td {
      background-color: #f4f9fd !important;
    }

    /* Hover row highlight */
    .table-hover > tbody > tr:hover > td {
      background-color: #daeaf7 !important;
    }

    /* Zoom button */
    #open_zoom {
      background: white !important;
      border: 1px solid #2a7ab8 !important;
      color: #2a7ab8 !important;
      border-radius: 4px !important;
      font-size: 12px !important;
      padding: 4px 12px !important;
    }
    #open_zoom:hover {
      background: #e8f4fd !important;
    }

    /* Slider input track color */
    .irs-bar, .irs-bar-edge {
      background: #2a7ab8 !important;
      border-top-color: #2a7ab8 !important;
      border-bottom-color: #2a7ab8 !important;
    }
    .irs-single {
      background: #2a7ab8 !important;
    }

    /* Compact hr separators */
    hr { margin: 6px 0; border-color: #dee2e6; }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('update_zoom_rulers', function(msg) {
      var scale = msg.zoom_pct / 100;
      var tick_px = 96 * scale / 4;  // CSS pixels per quarter-inch at current zoom

      // Horizontal ruler
      var hc = document.getElementById('zoom-ruler-h');
      if (hc) {
        var ctx = hc.getContext('2d');
        ctx.clearRect(0, 0, hc.width, hc.height);
        ctx.strokeStyle = '#666'; ctx.fillStyle = '#333';
        ctx.font = '8px sans-serif';
        for (var i = 0; i * tick_px < hc.width + tick_px; i++) {
          var x = Math.round(i * tick_px);
          var h = (i % 4 === 0) ? 14 : (i % 2 === 0) ? 9 : 5;
          ctx.beginPath(); ctx.moveTo(x, hc.height); ctx.lineTo(x, hc.height - h); ctx.stroke();
          if (i > 0 && i % 4 === 0) ctx.fillText(i/4 + '\"', x + 2, 10);
        }
      }

      // Vertical ruler
      var vc = document.getElementById('zoom-ruler-v');
      if (vc) {
        var ctx2 = vc.getContext('2d');
        ctx2.clearRect(0, 0, vc.width, vc.height);
        ctx2.strokeStyle = '#666'; ctx2.fillStyle = '#333';
        ctx2.font = '8px sans-serif';
        for (var j = 0; j * tick_px < vc.height + tick_px; j++) {
          var y = Math.round(j * tick_px);
          var w = (j % 4 === 0) ? 14 : (j % 2 === 0) ? 9 : 5;
          ctx2.beginPath(); ctx2.moveTo(vc.width, y); ctx2.lineTo(vc.width - w, y); ctx2.stroke();
          if (j > 0 && j % 4 === 0) ctx2.fillText(j/4 + '\"', 2, y - 2);
        }
      }
    });

    $(document).on('shown.bs.modal', function() {
      Shiny.setInputValue('modal_shown', Math.random());
    });

    $(document).on('click', '#open_zoom', function() {
      var el = document.documentElement;
      if      (el.requestFullscreen)       el.requestFullscreen();
      else if (el.webkitRequestFullscreen) el.webkitRequestFullscreen();
      else if (el.mozRequestFullScreen)    el.mozRequestFullScreen();
      else if (el.msRequestFullscreen)     el.msRequestFullscreen();
    });

    $(document).on('hidden.bs.modal', function() {
      if (document.fullscreenElement ||
          document.webkitFullscreenElement ||
          document.mozFullScreenElement ||
          document.msFullscreenElement) {
        if      (document.exitFullscreen)       document.exitFullscreen();
        else if (document.webkitExitFullscreen) document.webkitExitFullscreen();
        else if (document.mozCancelFullScreen)  document.mozCancelFullScreen();
        else if (document.msExitFullscreen)     document.msExitFullscreen();
      }
    });

    $(document).on('fullscreenchange webkitfullscreenchange mozfullscreenchange MSFullscreenChange', function() {
      var isFullscreen = document.fullscreenElement ||
                         document.webkitFullscreenElement ||
                         document.mozFullScreenElement ||
                         document.msFullscreenElement;
      if (!isFullscreen) {
        var $modal = $('.modal.show, .modal.in');
        if ($modal.length) $modal.modal('hide');
      }
    });
  ")),
  tags$div(
    id = "top-bar",
    shinyFiles::shinyFilesButton(
      "file",
      label    = "Load CSV",
      title    = "Select a CSV data file",
      multiple = FALSE
    ),
    tags$div(
      id = "top-bar-filename",
      textOutput("app_title", inline = TRUE)
    ),
    tags$div(
      id = "top-bar-run-label",
      "Run:"
    ),
    tags$div(
      id = "top-bar-run-wrapper",
      selectInput("run_id", "Run", choices = character(0)),
      tags$div(
        id = "top-bar-run-abbrev",
        textOutput("app_run_label", inline = TRUE)
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      conditionalPanel(
        condition = "output.data_loaded",
        tags$details(
          open = "open",
          tags$summary(style = "cursor: pointer;",
                       "Variable & Plot"),
          tags$div(
            style = "margin-top: 6px;",
            selectInput("variable", "Variable",
              choices  = list("Frequency" = "frequency", "Amplitude" = "amplitude", "Width" = "width"),
              selected = "frequency"
            ),
            radioButtons("plot_type", "Plot type",
              choices  = list("Baseline" = "baseline", "Before/After" = "ba"),
              selected = "baseline"
            ),
            conditionalPanel(
              condition = "input.plot_type == 'ba'",
              selectInput("treatment", "Treatment",
                choices = list("Senktide" = "senktide", "TTX" = "TTX"), selected = "senktide"
              )
            )
          )
        ),
        tags$details(
          tags$summary(style = "cursor: pointer;",
                       "Summary Statistics"),
          tags$div(
            style = "margin-top: 6px;",
            radioButtons("stat_type", "Statistic",
              choices  = list("Mean ± SE" = "mean_se", "Median + IQR" = "median_iqr"),
              selected = "mean_se"
            ),
            conditionalPanel(
              condition = "input.plot_type == 'baseline'",
              radioButtons("emm_type", "Contrast type",
                choices  = list(
                  "All pairs (4 groups)"       = "all_pairs",
                  "Main effects (sex & gonad)" = "main_effects"
                ),
                selected = "all_pairs"
              )
            ),
            conditionalPanel(
              condition = "input.plot_type == 'baseline' && input.emm_type == 'all_pairs'",
              tags$hr(style = "margin: 6px 0;"),
              checkboxGroupInput("kept_contrasts", "Pairwise contrasts",
                choices  = contrast_choices,
                selected = c("DI_OVX", "MALE_ORX", "DI_MALE", "OVX_ORX")
              ),
              radioButtons("adjust_family", "Adjustment family",
                choices  = list(
                  "Restricted set (recommended)" = "restricted",
                  "All 6 pairs (filter after)"   = "filter_after"
                ),
                selected = "restricted"
              ),
              tags$div(
                style = paste(
                  "font-size: 10.5px; color: #666; line-height: 1.4;",
                  "padding: 4px 6px; margin-top: 2px;",
                  "background: #f7f8fa; border-left: 3px solid #b8b8b8;",
                  "border-radius: 2px;"
                ),
                tags$em("Restricted: family = checked contrasts only. ",
                        "Filter after: family = all 6.")
              )
            )
          )
        ),
        tags$details(
          tags$summary(style = "cursor: pointer;",
                       "Y Axis"),
          tags$div(
            style = "margin-top: 6px;",
            selectInput("y_scale", "Scale",
              choices = c("pseudolog", "log", "sqrt", "normal"),
              selected = NULL
            ),
            numericInput("y_min", "Y min", value = NULL, step = NA),
            numericInput("y_max", "Y max", value = NULL, step = NA),
            tags$div(
              actionButton("y_apply", "Apply",
                style = "font-size:11px; padding:3px 10px; margin-top:4px;"),
              actionLink("y_reset", "Reset to default",
                style = "font-size:10px; color:#888; margin-left:8px;")
            ),
            radioButtons("y_scope", NULL,
              choices  = c("This variable" = "variable", "All variables" = "all"),
              selected = "variable",
              inline   = TRUE
            )
          )
        ),
        tags$details(
          tags$summary(style = "cursor: pointer;",
                       "Annotations"),
          tags$div(
            style = "margin-top: 6px;",
            radioButtons("method", "Display method",
              choices  = list("None" = "none", "Bracket" = "bracket", "CLD" = "CLD"),
              selected = "bracket"
            ),
            conditionalPanel(
              condition = "input.method == 'bracket'",
              radioButtons("bracket_mode", "Show comparisons",
                choices  = list("All significant" = "all", "One at a time" = "one"),
                selected = "all"
              )
            ),
            conditionalPanel(
              condition = "input.method == 'bracket' && input.bracket_mode == 'one' && input.plot_type == 'baseline'",
              radioButtons("contrast", "Comparison",
                choices  = contrast_choices,
                selected = character(0)
              )
            ),
            conditionalPanel(
              condition = "input.method == 'bracket' && input.bracket_mode == 'one' && input.plot_type == 'ba'",
              radioButtons("ba_group", "Group",
                choices  = c("DI", "OVX", "MALE", "ORX"),
                selected = "DI"
              )
            ),
            conditionalPanel(
              condition = "input.method == 'bracket'",
              radioButtons("p_label", "Label",
                choices  = list("Stars" = "stars", "P-value" = "p.value"),
                selected = "stars"
              ),
              sliderInput("alpha", "Show if p <",
                min = 0.001, max = 0.1, value = 0.05, step = 0.001
              )
            ),
            conditionalPanel(
              condition = "input.method == 'CLD'",
              radioButtons("cld_adjust", "Adjustment",
                choices  = list("Sidak" = "sidak", "Bonferroni" = "bonferroni", "None" = "none"),
                selected = "sidak"
              )
            )
          )
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 2px;"),
        tags$details(
          tags$summary(style = "cursor: pointer;", "Figure Size"),
          tags$div(
            style = "margin-top: 8px;",
            radioButtons("fig_unit", NULL,
              choices  = list("inches" = "in", "cm" = "cm"),
              selected = "in",
              inline   = TRUE
            ),
            fluidRow(
              column(6, numericInput("fig_width",  "Width",  value = 3.4, min = 1, max = 24, step = 0.5)),
              column(6, numericInput("fig_height", "Height", value = 2.7, min = 1, max = 24, step = 0.5))
            ),
            numericInput("fig_dpi", "DPI", value = 300, min = 72, max = 600, step = 50)
          )
        ),
        tags$hr(style = "margin-top: 8px; margin-bottom: 2px;"),
        tags$details(
          tags$summary(style = "cursor: pointer;", "Export"),
          tags$div(
            style = "margin-top: 8px; display: flex; flex-wrap: wrap; gap: 4px;",
            downloadButton("dl_png",  "PNG",  style = "padding: 4px 8px; font-size: 12px;"),
            downloadButton("dl_pdf",  "PDF",  style = "padding: 4px 8px; font-size: 12px;"),
            downloadButton("dl_svg",  "SVG",  style = "padding: 4px 8px; font-size: 12px;"),
            downloadButton("dl_tiff", "TIFF", style = "padding: 4px 8px; font-size: 12px;"),
            downloadButton("dl_eps",  "EPS",  style = "padding: 4px 8px; font-size: 12px;"),
            tags$hr(style = "width: 100%; margin: 4px 0; border: none; border-top: 1px solid #ccc;"),
            downloadButton("dl_summary", "Summary PDF",
                           style = "padding: 4px 8px; font-size: 12px; width: 100%;"),
            tags$div(
              style = "margin-top: 6px;",
              radioButtons("summary_pages", NULL,
                choices  = list("One page" = "one", "Two pages (plot | tables)" = "two"),
                selected = "one",
                inline   = FALSE
              )
            )
          )
        )
      )
    ),
    mainPanel(
      width = 10,
      imageOutput("plot", width = "100%"),
      conditionalPanel(
        condition = "output.data_loaded",
        tags$div(
          style = "margin-top: 4px;",
          actionButton("open_zoom", "Zoom view...",
                       style = "font-size: 12px; padding: 4px 12px;")
        )
      ),
      tags$div(
        style = "margin-top: 8px;",
        tags$hr(),
        # Fixed-width flex row: LMM | CLD letters | CLD key — positions never shift
        tags$div(
          style = "display: flex; align-items: flex-start; gap: 16px; flex-wrap: wrap;",
          tags$div(style = "flex: 0 0 300px;", tableOutput("lmm_table")),
          tags$div(style = "flex: 0 0 200px;", tableOutput("cld_table")),
          tags$div(
            style = paste(
              "background: #f7f8fa; border: 1px solid #dee2e6;",
              "border-radius: 4px; padding: 8px 12px;",
              "font-size: 11px; color: #555; line-height: 1.6;",
              "min-width: 140px; flex: 0 1 auto;",
              "align-self: flex-start; margin-top: 20px;"
            ),
            conditionalPanel(
              condition = "output.data_loaded",
              tags$strong("CLD letters"),
              tags$br(),
              "Groups sharing a letter are not",
              tags$br(),
              "significantly different.",
              tags$br(),
              "No common letter = p < α."
            )
          )
        ),
        uiOutput("warnings_ui"),
        conditionalPanel(
          condition = "output.data_loaded",
          tags$hr(),
          tableOutput("pairs_table")
        )
        ,
        conditionalPanel(
          condition = "output.data_loaded",
          tags$details(
            style = "margin-top: 8px;",
            tags$summary(
              style = "cursor: pointer; font-size: 11px; color: #888; user-select: none;",
              "Statistical methods"
            ),
            tags$div(
              style = paste(
                "font-size: 11px; color: #555; line-height: 1.65;",
                "padding: 6px 4px 10px 4px; max-width: 700px;"
              ),
              tags$p(
                tags$strong("Linear Mixed Model (LMM): "),
                "Fitted using ", tags$code("afex::mixed(method='KR')"),
                " with Kenward-Roger degree-of-freedom approximation.",
                " Fixed effects: sex × gonads (baseline) or sex × gonads × treatment (before/after).",
                " Random intercepts for slice (1|slice_id) and mouse (1|mouse_id) account for the nested",
                " repeated-measures design.",
                " Responses are transformed before fitting: √frequency, log₁₀(amplitude), log₁₀(width).",
                " The ANOVA table reports Type III F-tests."
              ),
              tags$p(
                tags$strong("Post-hoc comparisons: "),
                "Estimated marginal means (EMMs) computed with ", tags$code("emmeans::emmeans()"),
                " on the model (transformed) scale.",
                " Pairwise contrasts tested with t-statistics; p-values adjusted by Sidak (default)",
                " or as selected.",
                " CLD letters group means that are not significantly different —",
                " groups sharing a letter have p > α."
              )
            )
          )
        )
      )
    )
  )
)

# ---- Server --------------------------------------------------------------

server <- function(input, output, session) {

  app <- reactiveValues(
    raw            = NULL,
    filename       = NULL,
    available_runs = character(0),
    active_run     = DATA_RUN,
    base_lmms      = NULL,
    base_emms      = NULL,
    base_emms_main = NULL,
    sig_defs       = NULL,
    ba_lmms        = list(),
    base_warnings  = list(),
    ba_warnings    = list(),
    loaded         = FALSE,
    y_overrides    = list()
  )

  output$data_loaded <- reactive(app$loaded)
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # ---- shinyFiles browser setup ------------------------------------------

  # Roots for the file browser: 04-data first, then project root, then home
  file_roots <- reactive({
    r <- c()
    data_dir <- tryCatch(here::here("04-data"), error = function(e) NULL)
    if (!is.null(data_dir) && dir.exists(data_dir))
      r <- c(r, `04-data` = data_dir)
    proj_dir <- tryCatch(here::here(), error = function(e) NULL)
    if (!is.null(proj_dir) && dir.exists(proj_dir))
      r <- c(r, Project = proj_dir)
    last_dir <- getOption("lme_app_last_dir")
    if (!is.null(last_dir) && dir.exists(last_dir)) {
      existing_paths <- normalizePath(r, mustWork = FALSE)
      if (!normalizePath(last_dir, mustWork = FALSE) %in% existing_paths)
        r <- c(r, `Last used` = last_dir)
    }
    if (length(r) == 0) r <- c(Home = path.expand("~"))
    r
  })

  shinyFiles::shinyFileChoose(input, "file",
    roots     = file_roots,
    filetypes = c("csv"),
    session   = session
  )

  output$selected_file_path <- renderText({
    if (is.integer(input$file) || is.null(input$file)) return("No file selected")
    fi <- tryCatch(
      shinyFiles::parseFilePaths(file_roots(), input$file),
      error = function(e) NULL
    )
    if (is.null(fi) || nrow(fi) == 0) return("No file selected")
    as.character(fi$datapath)
  })

  # ---- Shared LMM fitting logic ------------------------------------------

  fit_for_run <- function(df_raw, run_id) {
    setProgress(0.15, "Preparing baseline data")
    df_base <- prep_baseline(df_raw, run_id)
    setProgress(0.40, "Fitting baseline LMMs (slow)...")

    warn_freq <- character(0)
    lmm_freq  <- NULL
    emm_freq  <- NULL
    emm_freq_sex    <- NULL
    emm_freq_gonads <- NULL
    warn_freq <- capture_msgs({
      set_sum_contrasts()
      lmm_freq        <- mixed(sqrt(freq) ~ sex * gonads + (1|slice_id) + (1|mouse_id),
                               data = df_base, method = "KR")
      emm_freq        <- emmeans(lmm_freq, ~ sex * gonads)
      emm_freq_sex    <- emmeans(lmm_freq, "sex")
      emm_freq_gonads <- emmeans(lmm_freq, "gonads")
    })

    warn_amp <- character(0)
    lmm_amp  <- NULL
    emm_amp  <- NULL
    emm_amp_sex    <- NULL
    emm_amp_gonads <- NULL
    df_amp   <- dplyr::filter(df_base, !is.na(amp_median))
    warn_amp <- capture_msgs({
      set_sum_contrasts()
      lmm_amp        <- mixed(log10(amp_median) ~ sex * gonads + (1|slice_id) + (1|mouse_id),
                               data = df_amp, method = "KR")
      emm_amp        <- emmeans(lmm_amp, ~ sex * gonads)
      emm_amp_sex    <- emmeans(lmm_amp, "sex")
      emm_amp_gonads <- emmeans(lmm_amp, "gonads")
    })

    warn_width <- character(0)
    lmm_width  <- NULL
    emm_width  <- NULL
    emm_width_sex    <- NULL
    emm_width_gonads <- NULL
    df_width   <- dplyr::filter(df_base, !is.na(width_median))
    warn_width <- capture_msgs({
      set_sum_contrasts()
      lmm_width        <- mixed(log10(width_median) ~ sex * gonads + (1|slice_id) + (1|mouse_id),
                                 data = df_width, method = "KR")
      emm_width        <- emmeans(lmm_width, ~ sex * gonads)
      emm_width_sex    <- emmeans(lmm_width, "sex")
      emm_width_gonads <- emmeans(lmm_width, "gonads")
    })

    setProgress(0.85, "Computing emmeans")
    lmms      <- list(freq = lmm_freq, amp = lmm_amp, width = lmm_width)
    emms      <- list(frequency = emm_freq, amplitude = emm_amp, width = emm_width)
    emms_main <- list(
      frequency = list(sex = emm_freq_sex,    gonads = emm_freq_gonads),
      amplitude = list(sex = emm_amp_sex,     gonads = emm_amp_gonads),
      width     = list(sex = emm_width_sex,   gonads = emm_width_gonads)
    )
    list(lmms = lmms, emms = emms, emms_main = emms_main,
         warnings = list(
           frequency = unique(warn_freq),
           amplitude = unique(warn_amp),
           width     = unique(warn_width)
         ),
         sig_defs = list(
           frequency = sig_contrasts_for(emms$frequency),
           amplitude = sig_contrasts_for(emms$amplitude),
           width     = sig_contrasts_for(emms$width)
         ))
  }

  # ---- File input --------------------------------------------------------

  observeEvent(input$file, {
    req(!is.integer(input$file))
    fi <- shinyFiles::parseFilePaths(file_roots(), input$file)
    req(nrow(fi) > 0)
    filepath <- as.character(fi$datapath)
    options(lme_app_last_dir = dirname(filepath))
    withProgress(message = "Loading data...", value = 0, {
      setProgress(0.05, "Reading CSV")
      df_raw <- load_raw(filepath)

      runs        <- sort(unique(df_raw$run_id))
      default_run <- if (DATA_RUN %in% runs) DATA_RUN else runs[1]

      fit <- fit_for_run(df_raw, default_run)

      app$raw            <- df_raw
      app$filename       <- basename(filepath)
      app$available_runs <- runs
      app$active_run     <- default_run
      app$base_lmms      <- fit$lmms
      app$base_emms      <- fit$emms
      app$base_emms_main <- fit$emms_main
      app$sig_defs       <- fit$sig_defs
      app$base_warnings  <- fit$warnings
      app$ba_lmms        <- list()
      app$ba_warnings    <- list()
      app$loaded         <- TRUE

      updateSelectInput(session, "run_id", choices = runs, selected = default_run)
      setProgress(1, "Done")
    })
  })

  # ---- Run selection change (re-fit LMMs) --------------------------------

  observeEvent(input$run_id, {
    req(app$loaded, nzchar(input$run_id), input$run_id != app$active_run)
    app$loaded  <- FALSE
    app$ba_lmms <- list()
    withProgress(message = paste("Switching to run:", input$run_id), value = 0, {
      fit <- fit_for_run(app$raw, input$run_id)
      app$active_run     <- input$run_id
      app$base_lmms      <- fit$lmms
      app$base_emms      <- fit$emms
      app$base_emms_main <- fit$emms_main
      app$sig_defs       <- fit$sig_defs
      app$base_warnings  <- fit$warnings
      app$ba_warnings    <- list()
      app$loaded         <- TRUE
      setProgress(1, "Done")
    })
  }, ignoreNULL = TRUE)

  output$app_title <- renderText({
    if (!is.null(app$filename)) app$filename else ""
  })

  output$app_run_label <- renderText({
    if (!is.null(app$active_run) && nzchar(app$active_run))
      paste0("≈ ", abbreviate_run(app$active_run))
    else ""
  })

  output$sidebar_filename <- renderText({
    if (!is.null(app$filename)) app$filename else ""
  })

  # ---- Lazy BA LMM fitting (only when stats requested) -------------------

  observeEvent(list(input$plot_type, input$treatment, input$method, app$loaded), {
    req(app$loaded, input$plot_type == "ba", input$method != "none")
    key <- input$treatment
    if (is.null(app$ba_lmms[[key]])) {
      withProgress(message = paste("Fitting", key, "LMMs (slow)..."), value = 0.5, {
        df_ba   <- prep_before_after(app$raw, key, app$active_run)
        result  <- NULL
        ba_warn <- character(0)
        ba_warn <- capture_msgs({
          result <- tryCatch(
            fit_ba_lmms(df_ba),
            error = function(e) {
              showNotification(paste("LMM fitting failed:", e$message),
                               type = "error", duration = 10)
              NULL
            }
          )
        })
        app$ba_lmms[[key]]    <- result
        app$ba_warnings[[key]] <- unique(ba_warn)
        setProgress(1, "Done")
      })
    }
  }, ignoreNULL = TRUE)

  # ---- Preselect significant baseline contrast ---------------------------

  observeEvent(list(input$variable, input$method, input$plot_type,
                    input$kept_contrasts, app$sig_defs), {
    req(app$loaded, input$method == "bracket", input$plot_type == "baseline",
        input$bracket_mode == "one")
    # Restrict choices to checked contrasts so the user can't request a
    # comparison that has been excluded from the family.
    kept_ids <- effective_kept_contrasts()
    kept_choices <- contrast_choices[
      vapply(contrast_choices, function(v) v %in% kept_ids, logical(1))
    ]
    if (length(kept_choices) == 0) kept_choices <- contrast_choices
    defs <- app$sig_defs[[input$variable]]
    # Default to first significant contrast that's also checked; otherwise the
    # first checked contrast.
    sig_kept <- intersect(defs, kept_ids)
    sel <- if (length(sig_kept) > 0) sig_kept[1]
           else if (length(kept_choices) > 0) unname(unlist(kept_choices[1]))
           else character(0)
    updateRadioButtons(session, "contrast",
      choices  = kept_choices,
      selected = sel
    )
  }, ignoreNULL = TRUE)

  # ---- Y-axis controls: sync to current variable/run --------------------

  observe({
    req(app$loaded, input$variable)
    vs <- effective_vs(input$variable, app$active_run, app$y_overrides)
    updateSelectInput(session,  "y_scale", selected = vs$scaleYtype)
    updateNumericInput(session, "y_min",   value    = vs$plot_y_min)
    updateNumericInput(session, "y_max",   value    = vs$plot_y_max)
  })

  observeEvent(input$y_apply, {
    req(app$loaded, input$y_scale, input$y_min, input$y_max)
    run <- app$active_run
    ov  <- list(scaleYtype = input$y_scale, y_min = input$y_min, y_max = input$y_max)
    if (input$y_scope == "variable") {
      key <- paste0(run, "/", input$variable)
      ovs <- app$y_overrides
      ovs[[key]] <- ov
      app$y_overrides <- ovs
    } else {
      ovs <- app$y_overrides
      for (v in names(var_specs)) {
        key <- paste0(run, "/", v)
        if (v == input$variable) {
          ovs[[key]] <- ov
        } else {
          cur <- effective_vs(v, run, app$y_overrides)
          ovs[[key]] <- list(scaleYtype = input$y_scale,
                             y_min      = cur$plot_y_min,
                             y_max      = cur$plot_y_max)
        }
      }
      app$y_overrides <- ovs
    }
  })

  observeEvent(input$y_reset, {
    req(app$loaded, input$variable)
    key <- paste0(app$active_run, "/", input$variable)
    ovs <- app$y_overrides
    ovs[[key]] <- NULL
    app$y_overrides <- ovs
    vs <- var_specs[[input$variable]]
    updateSelectInput(session,  "y_scale", selected = vs$scaleYtype)
    updateNumericInput(session, "y_min",   value    = vs$plot_y_min)
    updateNumericInput(session, "y_max",   value    = vs$plot_y_max)
  })

  # ---- Plot data (no LMM dependency) ------------------------------------

  plot_data <- reactive({
    req(app$loaded)
    col  <- var_specs[[input$variable]]$roi_col
    stat <- if (is.null(input$stat_type)) "mean_se" else input$stat_type
    if (input$plot_type == "baseline") {
      df     <- prep_baseline(app$raw, app$active_run)
      slices <- make_slice_summary(df, col)
      groups <- make_group_summary(slices, stat)
      if (stat == "median_iqr") { slices$s_mean <- slices$s_median; groups$g_mean <- groups$g_median }
      list(df = df, slices = slices, groups = groups,
           colors = rep(slice_colors_v2, length.out = nlevels(df$local_slice)))
    } else {
      df     <- prep_before_after(app$raw, input$treatment, app$active_run)
      slices <- make_slice_summary(df, col)
      groups <- make_group_summary(slices, stat)
      if (stat == "median_iqr") { slices$s_mean <- slices$s_median; groups$g_mean <- groups$g_median }
      list(df = df, slices = slices, groups = groups,
           colors = rep(slice_colors_v2, length.out = nlevels(df$local_slice)))
    }
  })

  # ---- Adjustment method (shared by CLD and pairwise table) -------------

  active_adjust <- reactive({
    if (isTRUE(input$method == "CLD")) input$cld_adjust else "sidak"
  })

  # ---- Restricted-contrast resolution -----------------------------------

  # Resolve which pair_ids to use in baseline mode. If the user has unchecked
  # everything we silently fall back to all 6, which is safer than letting the
  # contrast/CLD machinery error out. Notification is fired separately (see
  # observer below) so that this reactive remains side-effect-free and can be
  # called freely from plot/table reactives without spamming the user.
  effective_kept_contrasts <- reactive({
    sel <- input$kept_contrasts
    all_ids <- names(contrast_pairs)
    if (is.null(sel) || length(sel) == 0) return(all_ids)
    intersect(all_ids, sel)  # preserve canonical order from contrast_pairs
  })

  # Notify exactly once when the user goes from "some checked" to "none
  # checked", rather than on every downstream reactive evaluation.
  observeEvent(input$kept_contrasts, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (is.null(input$kept_contrasts) || length(input$kept_contrasts) == 0) {
      showNotification(
        "No contrasts checked \u2014 falling back to all 6 pairs.",
        type = "warning", duration = 4
      )
    }
  })

  effective_family_mode <- reactive({
    if (is.null(input$adjust_family)) "restricted" else input$adjust_family
  })

  active_warnings <- reactive({
    req(app$loaded)
    raw <- if (input$plot_type == "baseline") {
      app$base_warnings[[input$variable]]
    } else {
      app$ba_warnings[[input$treatment]]
    }
    translate_warnings(if (is.null(raw)) character(0) else raw)
  })

  # ---- Active LMM --------------------------------------------------------

  active_lmm <- reactive({
    req(app$loaded)
    lmm_key <- var_specs[[input$variable]]$lmm_key
    if (input$plot_type == "baseline") {
      app$base_lmms[[lmm_key]]
    } else {
      app$ba_lmms[[input$treatment]][[lmm_key]]  # NULL if not yet fitted
    }
  })

  # ---- Build annotated plot object ----------------------------------------

  build_annotated_plot <- reactive({
    req(app$loaded)
    pd <- plot_data()
    vs <- effective_vs(input$variable, app$active_run, app$y_overrides)

    # Bracket layout constants
    B_PAD  <- 0.03   # NPC gap above first bracket
    B_STEP <- 0.05   # NPC distance between bracket centers
    B_BOT  <- 0.02   # NPC gap between last bracket bottom and data top

    # Pre-count brackets so we can extend y_max before building the plot
    lmm_cur <- active_lmm()
    emm_type_cur <- if (is.null(input$emm_type)) "all_pairs" else input$emm_type
    # Resolve restricted-contrast inputs once (used by pre-count, brackets, CLD).
    # In BA mode, kept_ids is unused but harmless to evaluate.
    kept_ids_cur <- effective_kept_contrasts()
    family_cur   <- effective_family_mode()
    n_brk <- 0L
    if (input$method == "bracket") {
      if (input$plot_type == "baseline" && emm_type_cur == "all_pairs") {
        pairs_pre <- get_baseline_pairs_restricted(
          app$base_emms[[input$variable]], kept_ids_cur,
          adjust = "sidak", family_mode = family_cur
        )
        n_brk <- if (input$bracket_mode == "all")
          sum(!is.na(pairs_pre$group1) & pairs_pre$p.value <= input$alpha)
        else 1L
      } else if (input$plot_type == "ba" && !is.null(lmm_cur)) {
        ba_pre <- get_ba_treatment_pairs(lmm_cur)
        n_brk <- if (input$bracket_mode == "all")
          sum(ba_pre$p.value <= input$alpha)
        else 1L
      }
    }

    # NPC fraction to reserve at top; extend y_max to push data below that zone
    reserved      <- if (n_brk > 0) B_PAD + n_brk * B_STEP + B_BOT else 0
    plot_y_max_eff <- extend_y_for_brackets(vs$plot_y_min, vs$plot_y_max,
                                            reserved, vs$scaleYtype)
    # NPC y for each bracket (top to bottom)
    y_top_npc <- 1.0 - B_PAD
    y_step_npc <- B_STEP

    ticks_with_max <- if (!is.finite(vs$plot_y_max) || vs$plot_y_max == 0 ||
                          vs$plot_y_max %in% vs$forceYticks) {
      vs$forceYticks
    } else {
      sort(c(vs$forceYticks, vs$plot_y_max))
    }

    if (input$plot_type == "baseline") {

      p <- plot_baseline(
        df            = pd$df,
        slice_summary = pd$slices,
        group_summary = pd$groups,
        yresult       = pd$df[[vs$roi_col]],
        ylabel        = vs$ylabel,
        slice_colors  = pd$colors,
        plotType      = "quasirandom",
        scaleYtype    = vs$scaleYtype,
        plot_y_min    = vs$plot_y_min,
        plot_y_max    = plot_y_max_eff,
        forceYticks   = ticks_with_max,
        td_sigma      = vs$td_sigma,
        quasi_method  = "smiley"
      )
      p <- apply_app_theme_and_labels(p, "baseline")

      if (input$method == "bracket" && emm_type_cur == "all_pairs") {
        emm      <- app$base_emms[[input$variable]]
        pairs_df <- get_baseline_pairs_restricted(
          emm, kept_ids_cur, adjust = "sidak", family_mode = family_cur
        )
        # Use the gtable-based bracket renderer — bypasses ggplot's data
        # coordinate system so brackets sit reliably in the reserved NPC zone
        # at the top of the panel, regardless of y-axis transform. Returns a
        # gtable, which downstream rendering already handles.
        p <- add_baseline_brackets_gtable(
          p, pairs_df,
          mode         = input$bracket_mode,
          contrast_sel = input$contrast,
          p_label_type = input$p_label,
          alpha        = input$alpha,
          y_top        = y_top_npc,
          y_step       = y_step_npc
        )
      }

      if (input$method == "CLD") {
        emm <- app$base_emms[[input$variable]]
        if (emm_type_cur == "all_pairs") {
          alpha_cld <- if (!is.null(input$alpha)) input$alpha else 0.05
          pairs_df  <- get_baseline_pairs_restricted(
            emm, kept_ids_cur, adjust = input$cld_adjust, family_mode = family_cur
          )
          cld_df <- cld_from_restricted_pairs(emm, pairs_df, alpha = alpha_cld)
        } else {
          cld_df <- get_cld_df(emm, input$cld_adjust)
        }
        p <- p + geom_text(
          data        = cld_df,
          aes(x = group_id, y = plot_y_max_eff * 0.92, label = .group),
          inherit.aes = FALSE,
          size        = 5,
          fontface    = "bold"
        )
      }


    } else {

      p <- before_after_v6(
        df                = pd$df,
        slice_summary     = pd$slices,
        group_summary     = pd$groups,
        yresult           = pd$df[[vs$roi_col]],
        ylabel            = vs$ylabel,
        slice_colors      = pd$colors,
        plotType          = "quasirandom",
        scaleYtype        = vs$scaleYtype,
        hide_facet_titles = FALSE,
        hide_xlabels      = FALSE,
        plot_y_min        = vs$plot_y_min,
        plot_y_max        = plot_y_max_eff,
        forceYticks       = ticks_with_max,
        td_sigma          = vs$td_sigma,
        quasi_method      = "smiley"
      )
      p <- apply_app_theme_and_labels(p, "ba")

      lmm <- lmm_cur

      if (input$method == "bracket" && !is.null(lmm)) {
        ba_pairs <- get_ba_treatment_pairs(lmm)
        annot_df <- if (input$bracket_mode == "all") {
          dplyr::filter(ba_pairs, p.value <= input$alpha)
        } else {
          dplyr::filter(ba_pairs, p.value <= input$alpha, group_id == input$ba_group)
        }
        p <- add_ba_brackets_gtable(p, annot_df, input$treatment, input$p_label,
                                    y_npc = y_top_npc)
      }

      if (input$method == "CLD" && !is.null(lmm)) {
        emm_full <- emmeans(lmm, ~ sex * gonads * treatment)
        cld_df   <- get_cld_df(emm_full, input$cld_adjust)
        p <- p + geom_text(
          data        = cld_df,
          aes(x = as.character(treatment), y = plot_y_max_eff * 0.92, label = .group),
          inherit.aes = FALSE,
          size        = 4,
          fontface    = "bold"
        )
      }
    }

    p
  })

  # ---- Figure size: unit handling ---------------------------------------

  # The Width/Height numericInputs hold values in whatever unit `fig_unit`
  # currently selects. fig_dims_in() returns inches always, since all
  # downstream code (renderers, summary PDF, zoom rulers) operates in inches.
  CM_PER_IN <- 2.54

  fig_dims_in <- reactive({
    req(input$fig_width, input$fig_height, input$fig_unit)
    if (input$fig_unit == "cm") {
      list(w = input$fig_width / CM_PER_IN, h = input$fig_height / CM_PER_IN)
    } else {
      list(w = input$fig_width, h = input$fig_height)
    }
  })

  # When the unit toggle flips, rewrite the displayed values so the figure
  # stays the same physical size but the numbers match the new unit. Also
  # adjust min / max / step so the controls feel right at each unit.
  observeEvent(input$fig_unit, ignoreInit = TRUE, {
    if (input$fig_unit == "cm") {
      # Convert current displayed values from inches → cm.
      w_cm <- round(input$fig_width  * CM_PER_IN, 1)
      h_cm <- round(input$fig_height * CM_PER_IN, 1)
      updateNumericInput(session, "fig_width",
        value = w_cm, min = 2.5, max = 60, step = 1)
      updateNumericInput(session, "fig_height",
        value = h_cm, min = 2.5, max = 60, step = 1)
    } else {
      # cm → in
      w_in <- round(input$fig_width  / CM_PER_IN, 2)
      h_in <- round(input$fig_height / CM_PER_IN, 2)
      updateNumericInput(session, "fig_width",
        value = w_in, min = 1, max = 24, step = 0.5)
      updateNumericInput(session, "fig_height",
        value = h_in, min = 1, max = 24, step = 0.5)
    }
  })

  # ---- Render image for display -------------------------------------------

  output$plot <- renderImage({
    p   <- build_annotated_plot()
    dims <- fig_dims_in()
    tmp <- tempfile(fileext = ".png")
    render_to_file(tmp, "png", dims$w, dims$h, input$fig_dpi, p)
    px_w <- round(dims$w * 96)
    px_h <- round(dims$h * 96)
    list(src = tmp, width = px_w, height = px_h, alt = "Plot", deleteFile = TRUE)
  }, deleteFile = TRUE)

  # ---- Zoom modal ---------------------------------------------------------

  output$plot_zoom_modal <- renderImage({
    req(app$loaded)
    p    <- build_annotated_plot()
    dims <- fig_dims_in()
    tmp  <- tempfile(fileext = ".png")
    render_to_file(tmp, "png", dims$w, dims$h, input$fig_dpi, p)
    scale  <- input$zoom_pct / 100
    px_w   <- round(dims$w * 96 * scale)
    px_h   <- round(dims$h * 96 * scale)
    list(src = tmp, width = px_w, height = px_h, alt = "Zoom", deleteFile = TRUE)
  }, deleteFile = TRUE)

  observeEvent(input$open_zoom, {
    req(app$loaded)
    showModal(modalDialog(
      title = NULL,
      size  = "xl",
      easyClose = TRUE,
      fade  = FALSE,
      footer = modalButton("Close"),
      tags$div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 8px;",
        tags$span("Zoom:", style = "font-weight: bold;"),
        sliderInput("zoom_pct", NULL, min = 10, max = 1000, value = 100,
                    step = 10, width = "300px"),
        tags$span(textOutput("zoom_pct_label", inline = TRUE),
                  style = "min-width: 40px; font-size: 13px;")
      ),
      tags$div(
        style = "overflow: auto; max-height: 75vh; position: relative;",
        tags$div(
          style = "display: inline-block; position: relative;",
          # Horizontal ruler (above image)
          tags$div(
            style = "display: flex;",
            tags$div(style = "width: 24px; height: 20px; background: #e8e8e8; flex-shrink: 0;"),
            tags$canvas(id = "zoom-ruler-h", width = "2000", height = "20",
                        style = "background: #e8e8e8; display: block;")
          ),
          tags$div(
            style = "display: flex;",
            # Vertical ruler (left of image)
            tags$canvas(id = "zoom-ruler-v", width = "24", height = "2000",
                        style = "background: #e8e8e8; display: block; flex-shrink: 0;"),
            # Plot image
            imageOutput("plot_zoom_modal", width = "auto", height = "auto")
          )
        )
      )
    ))
  })

  output$zoom_pct_label <- renderText({
    req(input$zoom_pct)
    paste0(input$zoom_pct, "%")
  })

  # Update rulers whenever zoom changes (inside modal context)
  observe({
    req(input$zoom_pct, app$loaded)
    dims <- fig_dims_in()
    w_px <- round(dims$w * 96 * input$zoom_pct / 100)
    h_px <- round(dims$h * 96 * input$zoom_pct / 100)
    session$sendCustomMessage("update_zoom_rulers",
      list(zoom_pct = input$zoom_pct, img_w = w_px, img_h = h_px))
  })

  # Redraw rulers after modal DOM is ready
  observeEvent(input$modal_shown, {
    req(app$loaded, input$zoom_pct)
    dims <- fig_dims_in()
    w_px <- round(dims$w * 96 * input$zoom_pct / 100)
    h_px <- round(dims$h * 96 * input$zoom_pct / 100)
    session$sendCustomMessage("update_zoom_rulers",
      list(zoom_pct = input$zoom_pct, img_w = w_px, img_h = h_px))
  })

  # ---- Download handlers --------------------------------------------------

  make_dl <- function(fmt, ext = fmt) {
    downloadHandler(
      filename = function() {
        paste0(smart_export_name(app$filename, app$active_run,
                                 input$variable, input$plot_type,
                                 input$treatment), ".", ext)
      },
      content = function(file) {
        dims <- fig_dims_in()
        render_to_file(file, fmt, dims$w, dims$h, input$fig_dpi,
                       build_annotated_plot())
      }
    )
  }

  output$dl_png  <- make_dl("png")
  output$dl_pdf  <- make_dl("pdf")
  output$dl_svg  <- make_dl("svg")
  output$dl_tiff <- make_dl("tiff")
  output$dl_eps  <- make_dl("eps")

  output$dl_summary <- downloadHandler(
    filename = function() {
      paste0(smart_export_name(app$filename, app$active_run,
                               input$variable, input$plot_type,
                               input$treatment), "_summary.pdf")
    },
    content = function(file) {
      empty_df <- data.frame(`(not available)` = character(0), check.names = FALSE)
      p     <- build_annotated_plot()
      lmm   <- tryCatch(lmm_table_df(),   error = function(e) empty_df)
      cld   <- tryCatch(cld_table_df(),   error = function(e) empty_df)
      pairs <- tryCatch(pairs_table_df(), error = function(e) empty_df)
      ds_label  <- if (!is.null(app$filename)) extract_dataset(app$filename) else ""
      run_label <- if (!is.null(app$active_run) && nzchar(app$active_run))
                     abbreviate_run(app$active_run) else ""
      subtitle  <- paste(Filter(nzchar, c(ds_label, run_label)), collapse = " | ")
      title <- paste0(
        tools::toTitleCase(input$variable), "  —  ",
        if (input$plot_type == "ba") paste0(input$treatment, " (before/after)") else "Baseline",
        if (nzchar(subtitle)) paste0("\n", subtitle) else ""
      )
      dims <- fig_dims_in()
      make_summary_page(
        file      = file,
        plot_obj  = p,
        lmm_df    = lmm,
        cld_df    = cld,
        pairs_df  = pairs,
        fig_w     = dims$w,
        fig_h     = dims$h,
        title     = title,
        two_pages = isTRUE(input$summary_pages == "two"),
        warnings  = active_warnings()
      )
    }
  )

  # ---- Warning: plot height exceeds one-page fit --------------------------

  observe({
    req(input$fig_height, input$summary_pages, input$fig_unit)
    h_in <- fig_dims_in()$h
    if (h_in > 5.7 && input$summary_pages == "one") {
      showNotification(
        "Plot height exceeds 5.7\". Tables will be scaled/compressed to fit. Switch to 'Two pages' to preserve table readability.",
        type     = "warning",
        duration = 8
      )
    }
  })

  # ---- Table data reactives (shared by on-screen tables and summary export) ----

  lmm_table_df <- reactive({
    req(app$loaded)
    lmm <- active_lmm()
    req(!is.null(lmm))
    format_lmm_table(lmm)
  })

  cld_table_df <- reactive({
    req(app$loaded)
    adj <- active_adjust()
    if (input$plot_type == "baseline") {
      emm <- app$base_emms[[input$variable]]
      emm_type_sel <- if (is.null(input$emm_type)) "all_pairs" else input$emm_type
      # CLD restriction only applies to all_pairs; main_effects has no CLD anyway,
      # but if a user has selected CLD annotation while in main_effects we still
      # show the full-grid letters for completeness.
      if (emm_type_sel == "all_pairs") {
        kept_ids <- effective_kept_contrasts()
        family   <- effective_family_mode()
        # Use alpha from the bracket slider when available (so CLD letters and
        # bracket significance share the same threshold); fall back to 0.05.
        alpha_sel <- if (!is.null(input$alpha)) input$alpha else 0.05
        pairs_df  <- get_baseline_pairs_restricted(emm, kept_ids, adj, family)
        cld_df    <- cld_from_restricted_pairs(emm, pairs_df, alpha = alpha_sel)
      } else {
        cld_df <- get_cld_df(emm, adj)
      }
      data.frame(Group = cld_df$group_id, Letter = cld_df$.group,
                 stringsAsFactors = FALSE)
    } else {
      lmm <- active_lmm()
      req(!is.null(lmm))
      emm_full <- emmeans(lmm, ~ sex * gonads * treatment)
      cld_df   <- get_cld_df(emm_full, adj)
      cld_df   <- cld_df[order(match(cld_df$group_id, GROUPS), as.character(cld_df$treatment)), ]
      data.frame(Group     = cld_df$group_id,
                 Treatment = as.character(cld_df$treatment),
                 Letter    = cld_df$.group,
                 stringsAsFactors = FALSE)
    }
  })

  pairs_table_df <- reactive({
    req(app$loaded)
    adj <- active_adjust()
    if (input$plot_type == "baseline") {
      emm_type_sel <- if (is.null(input$emm_type)) "all_pairs" else input$emm_type
      if (emm_type_sel == "main_effects") {
        req(!is.null(app$base_emms_main))
        emms_v <- app$base_emms_main[[input$variable]]
        add_stars <- function(p) dplyr::case_when(
          p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"
        )
        sex_p <- as.data.frame(pairs(emms_v$sex,    adjust = adj))
        gon_p <- as.data.frame(pairs(emms_v$gonads, adjust = adj))
        sex_p$Effect <- "sex"; gon_p$Effect <- "gonads"
        both  <- rbind(sex_p, gon_p)
        data.frame(
          Effect    = both$Effect,
          Contrast  = both$contrast,
          Estimate  = round(both$estimate, 4),
          SE        = round(both$SE,       4),
          `t ratio` = round(both$t.ratio,  3),
          p         = formatC(both$p.value, format = "f", digits = 4),
          ` `       = add_stars(both$p.value),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      } else {
        emm      <- app$base_emms[[input$variable]]
        kept_ids <- effective_kept_contrasts()
        family   <- effective_family_mode()
        pairs_df <- get_baseline_pairs_restricted(emm, kept_ids, adj, family)
        format_pairs_table(pairs_df, "baseline")
      }
    } else {
      lmm <- active_lmm()
      req(!is.null(lmm))
      ba_pairs <- get_ba_treatment_pairs(lmm, adj)
      format_pairs_table(ba_pairs, "ba")
    }
  })

  # ---- Tables ------------------------------------------------------------

  output$lmm_table <- renderTable({
    lmm_table_df()
  }, striped = TRUE, hover = TRUE, caption = "LMM ANOVA", caption.placement = "top")

  output$cld_table <- renderTable({
    cld_table_df()
  }, striped = TRUE, hover = TRUE, caption = "CLD Letters", caption.placement = "top")

  output$pairs_table <- renderTable({
    pairs_table_df()
  }, striped = TRUE, hover = TRUE, caption = "Pairwise Comparisons", caption.placement = "top")

  output$warnings_ui <- renderUI({
    w <- active_warnings()
    if (length(w) == 0) return(NULL)
    tags$div(
      style = "margin-top: 6px; font-size: 11px; color: #856404; background: #fff8e1; border: 1px solid #ffe082; border-radius: 4px; padding: 6px 10px;",
      tags$strong("Model notes: "),
      tags$ul(style = "margin: 4px 0 0 0; padding-left: 18px;",
        lapply(w, function(msg) tags$li(msg))
      )
    )
  })
}

shinyApp(ui, server)
