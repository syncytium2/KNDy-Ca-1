# before-after-v7.R
#
# Successor to before_after_v6(). Uses helpers from plot-slices-core.R to
# share machinery with plot_baseline_v2. Adds optional per-slice color
# and shape mapping for stable visual identity, and abbreviates x-axis
# treatment labels for compact figures (b / s / TTX / first three letters).
#
# Backward-compatible signature: every original argument is preserved.
# New arguments are all optional with NULL defaults that fall back to v1
# behavior.

before_after_v7 <- function(
    df, slice_summary, group_summary, yresult, ylabel,
    slice_colors,                  # legacy: vector of colors, used when
                                   # slice_color_map is NULL
    plotType, scaleYtype,
    hide_facet_titles, hide_xlabels,
    plot_y_min, plot_y_max,
    forceYticks  = NULL,
    td_sigma     = NULL,
    quasi_method = "smiley",
    # New optional arguments:
    slice_color_map = NULL,        # named vector slice_id -> hex
    slice_shape_map = NULL,        # named vector slice_id -> integer
    apply_manuscript_style = TRUE,
    abbreviate_x_labels    = TRUE,
    nudge_x                = DEFAULT_NUDGE,
    # Boss's consistent whitespace rule: the slice-mean pair is shifted toward
    # baseline to save horizontal space. This is deliberate and consistent across
    # all 4 groups in her figure — it belongs in the code, not in Illustrator.
    # Default 0 = symmetric (current production behavior, all callers unaffected).
    # Set nudge_shift = 0.085 to reproduce boss's layout automatically.
    # Do NOT 'correct' this back to 0; if set, it is intentional.
    nudge_shift            = 0
) {
  if (is.null(td_sigma)) td_sigma <- DEFAULT_TD_SIGMA
  .nudge_base <- nudge_x - nudge_shift   # baseline side: shorter offset from x=1
  .nudge_trt  <- nudge_x + nudge_shift   # treatment side: longer offset from x=2

  # x_anchor for over-max annotation: first treatment level present in df.
  # Fixes v6 bug that hardcoded x="baseline".
  x_anchor <- if (is.factor(df$treatment)) levels(df$treatment)[1]
              else                         as.character(sort(unique(df$treatment)))[1]

  # Foundation
  if (!is.null(slice_color_map)) {
    viz <- ggplot(df, aes(x = treatment, y = yresult, color = slice_id))
  } else {
    viz <- ggplot(df, aes(x = treatment, y = yresult, color = local_slice))
  }

  # ROI swarm + over-max annotation
  viz <- viz +
    build_over_max_annotation(yresult, plot_y_max, x_anchor) +
    build_roi_swarm_layer(plotType, quasi_method = quasi_method)

  # Per-slice line + dots (aes depends on path)
  if (!is.null(slice_color_map)) {
    viz <- viz +
      geom_line(
        data = slice_summary,
        aes(x = treatment, y = s_mean, group = slice_id, color = slice_id),
        alpha = 0.7, size = DEFAULT_LINE_SIZE,
        position = position_nudge(x = c(.nudge_base, -.nudge_trt))
      ) +
      geom_point(
        data = slice_summary,
        aes(x = treatment, y = s_mean, fill = slice_id, shape = slice_id),
        size = DEFAULT_LARGE_DOT_SIZE,
        stroke = NA,
        position = position_nudge(x = c(.nudge_base, -.nudge_trt)),
        na.rm = TRUE
      )
  } else {
    viz <- viz +
      geom_line(
        data = slice_summary,
        aes(x = treatment, y = s_mean, group = local_slice, color = local_slice),
        alpha = 0.7, size = DEFAULT_LINE_SIZE,
        position = position_nudge(x = c(.nudge_base, -.nudge_trt))
      ) +
      geom_point(
        data = slice_summary,
        aes(x = treatment, y = s_mean,
            fill = local_slice, shape = local_slice),
        color = "black", size = DEFAULT_LARGE_DOT_SIZE,
        stroke = NA,
        position = position_nudge(x = c(.nudge_base, -.nudge_trt)),
        na.rm = TRUE
      )
  }

  # Group-level error bars and means (no nudge)
  viz <- viz +
    geom_errorbar(
      data = group_summary,
      aes(x = treatment, ymin = g_ymin, ymax = g_ymax),
      color = "black", width = 0.15, size = DEFAULT_LINE_SIZE,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = group_summary,
      aes(x = treatment, y = g_mean),
      color = "black", size = DEFAULT_LARGE_DOT_SIZE,
      stroke = 0, inherit.aes = FALSE, na.rm = TRUE
    ) +
    geom_line(
      data = group_summary,
      aes(x = treatment, y = g_mean, group = group_id),
      color = "black", size = DEFAULT_LINE_SIZE,
      inherit.aes = FALSE
    ) +
    facet_grid(. ~ group_id, scales = "free_x", space = "free_x")

  # Color / fill / shape scales
  if (!is.null(slice_color_map)) {
    shape_vals <- if (!is.null(slice_shape_map)) slice_shape_map
                  else setNames(rep(16L, length(slice_color_map)), names(slice_color_map))
    viz <- viz +
      scale_color_manual(values = slice_color_map) +
      scale_fill_manual( values = slice_color_map) +
      scale_shape_manual(values = shape_vals)
  } else {
    viz <- viz +
      scale_color_manual(values = slice_colors) +
      scale_fill_manual( values = slice_colors) +
      scale_shape_manual(values = rep(21L, length(slice_colors)))
  }

  # X-axis label abbreviation (b / s / TTX / first three)
  if (abbreviate_x_labels) {
    viz <- viz + scale_x_discrete(labels = abbrev_treatment_label)
  }

  # Labels, theme, y-scale
  viz <- viz +
    labs(x = NULL, y = ylabel) +
    build_slice_theme(hide_xlabels      = hide_xlabels,
                      hide_facet_titles = hide_facet_titles) +
    build_y_scale_layer(scaleYtype, plot_y_min, plot_y_max,
                        forceYticks, td_sigma)

  if (apply_manuscript_style) {
    viz <- apply_manuscript_styling(viz)
  }

  viz
}
