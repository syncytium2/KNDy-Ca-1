# plot-baseline-v2.R
#
# Successor to plot_baseline(). Uses helpers from plot-slices-core.R to
# remove duplication with before_after_v7. Adds optional per-slice color
# and shape mapping for stable visual identity across run_id and mode
# changes.
#
# Backward-compatible signature: every original argument is preserved.
# New arguments are all optional with NULL defaults that fall back to v1
# behavior.

plot_baseline_v2 <- function(
    df, slice_summary, group_summary, yresult, ylabel,
    slice_colors,                  # legacy: vector of colors, used when
                                   # slice_color_map is NULL
    plotType, scaleYtype,
    hide_xlabels   = FALSE,
    plot_y_min, plot_y_max,
    forceYticks    = NULL,
    td_sigma       = NULL,
    quasi_method   = "smiley",
    # New optional arguments:
    slice_color_map = NULL,        # named vector slice_id -> hex
    slice_shape_map = NULL,        # named vector slice_id -> integer
    apply_manuscript_style = TRUE,
    nudge_x         = DEFAULT_NUDGE
) {
  # td_sigma fallback matches v1
  if (is.null(td_sigma)) td_sigma <- DEFAULT_TD_SIGMA

  # Deterministic x_anchor for over-max annotation: first level of group_id.
  x_anchor <- if (is.factor(df$group_id)) levels(df$group_id)[1]
              else                        sort(unique(df$group_id))[1]

  # Foundation
  if (!is.null(slice_color_map)) {
    viz <- ggplot(df, aes(x = group_id, y = yresult, color = slice_id))
  } else {
    viz <- ggplot(df, aes(x = group_id, y = yresult, color = local_slice))
  }

  # ROI swarm + over-max annotation
  viz <- viz +
    build_over_max_annotation(yresult, plot_y_max, x_anchor) +
    build_roi_swarm_layer(plotType, quasi_method = quasi_method)

  # Slice-mean dots (aes depends on path)
  if (!is.null(slice_color_map)) {
    viz <- viz +
      geom_point(
        data = slice_summary,
        aes(x = group_id, y = s_mean, color = slice_id, fill = slice_id, shape = slice_id),
        size = DEFAULT_LARGE_DOT_SIZE,
        stroke = NA, inherit.aes = FALSE,
        position = position_nudge(x = nudge_x), na.rm = TRUE
      )
  } else {
    viz <- viz +
      geom_point(
        data = slice_summary,
        aes(x = group_id, y = s_mean, fill = local_slice, shape = local_slice),
        color = "black", size = DEFAULT_LARGE_DOT_SIZE,
        stroke = NA, inherit.aes = FALSE,
        position = position_nudge(x = nudge_x), na.rm = TRUE
      )
  }

  # Group-level error bars and means
  viz <- viz +
    geom_errorbar(
      data = group_summary,
      aes(x = group_id, ymin = g_ymin, ymax = g_ymax),
      color = "black", width = 0.15, size = DEFAULT_LINE_SIZE,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = group_summary,
      aes(x = group_id, y = g_mean),
      color = "black", size = DEFAULT_LARGE_DOT_SIZE,
      stroke = 0, inherit.aes = FALSE, na.rm = TRUE
    )

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

  # Labels, theme, y-scale
  viz <- viz +
    labs(x = NULL, y = ylabel) +
    build_slice_theme(hide_xlabels = hide_xlabels) +
    build_y_scale_layer(scaleYtype, plot_y_min, plot_y_max,
                        forceYticks, td_sigma)

  if (apply_manuscript_style) {
    viz <- apply_manuscript_styling(viz)
  }

  viz
}
