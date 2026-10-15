# plot-slices-core.R
#
# Shared helpers for slice-level scatter plots. Used by both
# plot_baseline_v2() and before_after_v7(). Lower-level than the wrappers —
# defines style constants, scale builders, geom builders, theme block, and
# the slice-id -> color/shape mapping used for stable visual identity.
#
# This file does NOT touch the v1 functions (plot_baseline, before_after_v6),
# which continue to coexist for the legacy app and for any notebooks already
# calling them.

# ---- Style constants ------------------------------------------------------
# Centralized so dot/line sizes can be tweaked in one place rather than in
# each wrapper. Wrappers reference these as defaults but can override.

DEFAULT_SMALL_DOT_SIZE <- 0.75
DEFAULT_LARGE_DOT_SIZE <- 1.5
DEFAULT_LINE_SIZE      <- 0.25
DEFAULT_SWARM_WIDTH    <- 0.3
DEFAULT_NUDGE          <- 0.35
DEFAULT_FONT_CHOICE    <- "Arial"
DEFAULT_FONT_SIZE      <- 11
DEFAULT_TOP_MARGIN     <- 0.5    # cm
DEFAULT_LEFT_MARGIN    <- 0.5    # cm
DEFAULT_TD_SIGMA       <- 0.001  # for pseudo_log_trans

# ---- Y-axis scale layer ---------------------------------------------------
# Replaces the 20-line switch duplicated in plot_baseline and before_after_v6.
# Returns a single ggproto layer that the caller adds with `+`.
#
# scaleYtype: one of "log", "pseudolog", "squareRoot", "normal"
# forceYticks: NULL (let ggplot decide) or numeric vector of break positions
# td_sigma: pseudo_log_trans sigma; default 0.001 matches v1 behavior

build_y_scale_layer <- function(scaleYtype, plot_y_min, plot_y_max,
                                forceYticks = NULL,
                                td_sigma    = DEFAULT_TD_SIGMA) {
  # Default break set used in pseudolog when no forceYticks are supplied —
  # matches v1's hardcoded list.
  default_breaks <- c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 1.3)

  if (is.null(forceYticks)) {
    switch(scaleYtype,
      log        = scale_y_log10(limits = c(plot_y_min, plot_y_max)),
      pseudolog  = scale_y_continuous(
                     trans  = pseudo_log_trans(sigma = td_sigma, base = 10),
                     breaks = default_breaks,
                     limits = c(plot_y_min, plot_y_max)),
      squareRoot = scale_y_continuous(
                     trans  = "sqrt",
                     limits = c(0, plot_y_max),
                     labels = scales::number_format(accuracy = 0.01)),
      normal     = scale_y_continuous(limits = c(0, plot_y_max))
    )
  } else {
    plotBreaks <- if (length(forceYticks) > 1) {
      forceYticks
    } else {
      pretty(c(plot_y_min, plot_y_max), 7)
    }
    switch(scaleYtype,
      log        = scale_y_log10(
                     limits = c(plot_y_min, plot_y_max),
                     breaks = plotBreaks),
      pseudolog  = scale_y_continuous(
                     trans  = pseudo_log_trans(sigma = td_sigma, base = 10),
                     limits = c(plot_y_min, plot_y_max),
                     breaks = plotBreaks),
      squareRoot = scale_y_continuous(
                     transform = "sqrt",
                     limits    = c(0, plot_y_max),
                     breaks    = plotBreaks,
                     labels    = scales::number_format(accuracy = 0.01)),
      normal     = scale_y_continuous(
                     limits = c(0, plot_y_max),
                     breaks = plotBreaks)
    )
  }
}

# ---- ROI swarm geom -------------------------------------------------------
# The "small dots" layer showing individual ROI values. plotType picks the
# distribution rendering style; quasirandom is the project default.

build_roi_swarm_layer <- function(plotType,
                                  dot_size     = DEFAULT_SMALL_DOT_SIZE,
                                  swarm_width  = DEFAULT_SWARM_WIDTH,
                                  quasi_method = "smiley") {
  switch(plotType,
    jitter      = geom_jitter(height = 0, width = 0.35, alpha = 0.5,
                              size = dot_size, stroke = 0, na.rm = TRUE),
    beeswarm    = geom_beeswarm(stroke = 0, cex = 3,
                                size = dot_size, alpha = 0.5, na.rm = TRUE),
    quasirandom = geom_quasirandom(method = quasi_method, alpha = 0.5,
                                   size = dot_size, stroke = 0,
                                   width = swarm_width, na.rm = TRUE)
  )
}

# ---- Over-max annotation --------------------------------------------------
# When ROI values exceed plot_y_max, annotates the count and max as a small
# label at x_anchor. Returns NULL when no overflow (caller can add NULL to
# a ggplot harmlessly).
#
# Fixes a v1 bug: plot_baseline hardcoded x="baseline" or levels()[1], and
# before_after_v6 hardcoded x="baseline" — both broke if those weren't
# present in the data. Now caller provides x_anchor explicitly.

build_over_max_annotation <- function(yresult, plot_y_max, x_anchor) {
  super_max <- max(yresult, na.rm = TRUE)
  if (!is.finite(super_max) || super_max <= plot_y_max) return(NULL)
  super_maxes <- yresult[yresult > plot_y_max]
  n_super_max <- length(super_maxes)
  annot <- paste("  mx:", num2str(super_max),
                 "n:",   num2str(n_super_max, 0),
                 "up:",  num2str(plot_y_max, 1))
  annotate("text", x = x_anchor, y = plot_y_max,
           label = annot, size = 1.5)
}

# ---- Theme block ----------------------------------------------------------
# Conservative theme: theme_bw + selective overrides. Mirrors v1 behavior
# so visual output is identical for the default-styled path. Future
# migration target for textTheme()/boxTheme() but kept minimal for now to
# avoid scope creep.

build_slice_theme <- function(font_size         = DEFAULT_FONT_SIZE,
                              font_choice       = DEFAULT_FONT_CHOICE,
                              top_margin        = DEFAULT_TOP_MARGIN,
                              left_margin       = DEFAULT_LEFT_MARGIN,
                              hide_xlabels      = FALSE,
                              hide_facet_titles = FALSE) {
  list(
    theme_bw(base_size = font_size, base_family = font_choice),
    theme(
      panel.border    = element_blank(),
      panel.grid      = element_blank(),
      legend.position = "none",
      strip.text.x    = if (hide_facet_titles) element_blank()
                        else element_text(size = font_size, family = font_choice),
      strip.background = if (hide_facet_titles) element_blank()
                         else element_rect(colour = "NA", fill = "NA"),
      axis.text.x     = if (hide_xlabels) element_blank() else NULL,
      plot.margin     = margin(top_margin, 0, 0, left_margin, unit = "cm")
    )
  )
}

# ---- X-axis treatment label abbreviator -----------------------------------
# Used by before_after_v7 via scale_x_discrete(labels = abbrev_treatment_label).
# Compact-figure convention: baseline -> "b", senktide -> "s", TTX kept as
# "TTX", anything else -> first three characters (case preserved on output).

abbrev_treatment_label <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    sl <- tolower(s)
    if      (sl == "baseline") "b"
    else if (sl == "senktide") "s"
    else if (sl == "ttx")      "TTX"
    else                       substr(s, 1, 3)
  }, character(1), USE.NAMES = FALSE)
}

# ---- Manuscript styling polish --------------------------------------------
# Tightens y-title-to-tick-label spacing for compact (~3.4" wide) figures.
# Negative margin pulls the title closer to the axis. Tweak r if too tight
# or too loose. Pure ggplot2 theme tweak — composable with + on any plot.

apply_manuscript_styling <- function(p) {
  p + theme(
    axis.title.y = element_text(margin = margin(r = -1))
  )
}

# ---- Slice aesthetic mapping ----------------------------------------------
# Build a stable per-slice (color, shape) mapping from a vector of slice_ids.
# Used to give the same slice the same visual identity within a session, so
# switching baseline <-> BA mode (or run_id within the same data file) keeps
# slices visually anchored.
#
# Returns a list with $colors and $shapes, both named character/integer
# vectors keyed by slice_id (as character).
#
# Stability properties:
#   - Deterministic: same input set -> identical output map.
#   - Order-independent: input order doesn't matter; we sort internally.
#   - Within-run robust: same slice_ids visible -> identical colors/shapes.
#   - Cross-run partial: slices that survive a filter keep their position
#     in the sorted vector, so most stay anchored. Slices that appear or
#     disappear can shift their neighbors.
#
# Algorithm: sort slice_ids; for slice at position i (0-based),
#   color = palette[(i %% N_colors) + 1]
# lookup_slice_aes: table-driven color/shape lookup against slice_color_table.csv.
# Crashes loudly if any (slice_id, group_id) pair is missing from the table,
# which means the canonical table needs to be regenerated via
# Rscript 03-data-prep/build-color-table.R
lookup_slice_aes <- function(slice_ids, group_ids, color_table) {
  query <- dplyr::distinct(
    data.frame(slice_id = as.character(slice_ids),
               group_id = as.character(group_ids),
               stringsAsFactors = FALSE)
  )
  joined <- dplyr::left_join(query, color_table, by = c("slice_id", "group_id"))

  missing <- joined[is.na(joined$color) | is.na(joined$shape), ]
  if (nrow(missing) > 0) {
    stop(sprintf(
      "Slice/group pairs not found in color table:\n%s\n\nRegenerate via: Rscript 03-data-prep/build-color-table.R",
      paste(sprintf("  %s / %s", missing$slice_id, missing$group_id), collapse = "\n")
    ))
  }

  list(
    colors = setNames(joined$color,         joined$slice_id),
    shapes = setNames(as.integer(joined$shape), joined$slice_id)
  )
}
