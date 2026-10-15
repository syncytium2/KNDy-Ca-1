plot_baseline <- function(df, slice_summary, group_summary, yresult, ylabel,
                          slice_colors,
                          plotType, scaleYtype,
                          hide_xlabels   = FALSE,
                          plot_y_min, plot_y_max,
                          forceYticks    = NULL,
                          td_sigma       = NULL,
                          quasi_method   = "smiley") {

  ylabel2 <- paste(ylabel, scaleYtype, "\n\n")
  ylabel2 <- ylabel

  summary_color  <- "black"
  small_dot_size <- 0.75
  large_dot_size <- 1.5
  line_size      <- 0.25
  font_choice    <- "Arial"
  font_size      <- 11
  left_margin    <- 0.5
  top_margin     <- 0.5
  swarm_width    <- 0.3
  nudge          <- 0.35

  viz <- ggplot(df, aes(x = group_id, y = yresult, color = local_slice))

  super_max <- max(yresult, na.rm = TRUE)
  if (super_max > plot_y_max) {
    super_maxes <- yresult[yresult > plot_y_max]
    n_super_max <- length(super_maxes)
    annot <- paste("  mx:", num2str(super_max), "n:", num2str(n_super_max, 0), "up:", num2str(plot_y_max, 1))
    viz <- viz + annotate("text", x = levels(df$group_id)[1], y = plot_y_max, label = annot, size = 1.5)
  }

  viz <- viz + switch(plotType,
    jitter      = geom_jitter(height = 0, width = 0.35, alpha = 0.5,
                              size = small_dot_size, stroke = 0, na.rm = TRUE),
    beeswarm    = geom_beeswarm(stroke = 0, cex = 3, size = small_dot_size, alpha = 0.5, na.rm = TRUE),
    quasirandom = geom_quasirandom(method = quasi_method, alpha = 0.5,
                                   size = small_dot_size, stroke = 0,
                                   width = swarm_width, na.rm = TRUE)
  )

  viz <- viz +
    geom_point(
      data = slice_summary,
      aes(x = group_id, y = s_mean, fill = local_slice),
      shape = 21, color = "black", size = large_dot_size,
      stroke = NA, inherit.aes = FALSE,
      position = position_nudge(x = nudge), na.rm = TRUE
    ) +
    geom_errorbar(
      data = group_summary,
      aes(x = group_id, ymin = g_ymin, ymax = g_ymax),
      color = summary_color,
      width = 0.15,
      size = line_size,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = group_summary,
      aes(x = group_id, y = g_mean),
      color = summary_color,
      size = large_dot_size,
      stroke = 0,
      inherit.aes = FALSE,
      na.rm = TRUE
    ) +
    scale_color_manual(values = slice_colors) +
    scale_fill_manual(values = slice_colors) +
    labs(x = NULL, y = ylabel2) +
    theme_bw(base_size = font_size, base_family = font_choice) +
    theme(
      panel.border    = element_blank(),
      panel.grid      = element_blank(),
      legend.position = "none",
      axis.text.x     = if (hide_xlabels) element_blank() else NULL,
      plot.margin     = margin(top_margin, 0, 0, left_margin, unit = "cm")
    )

  if (is.null(td_sigma)) td_sigma <- 0.001
  breaks <- c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 1.3)

  if (is.null(forceYticks)) {
    viz <- viz + switch(scaleYtype,
      log        = scale_y_log10(limits = c(plot_y_min, plot_y_max)),
      pseudolog  = scale_y_continuous(trans = pseudo_log_trans(sigma = td_sigma, base = 10),
                                      breaks = breaks,
                                      limits = c(plot_y_min, plot_y_max)),
      squareRoot = scale_y_continuous(trans = "sqrt", limits = c(0, plot_y_max),
                                      labels = scales::number_format(accuracy = 0.01)),
      normal     = scale_y_continuous(limits = c(0, plot_y_max))
    )
  } else {
    plotBreaks <- if (length(forceYticks) > 1) forceYticks else pretty(c(plot_y_min, plot_y_max), 7)
    viz <- viz + switch(scaleYtype,
      log        = scale_y_log10(limits = c(plot_y_min, plot_y_max), breaks = plotBreaks),
      pseudolog  = scale_y_continuous(trans = pseudo_log_trans(sigma = td_sigma, base = 10),
                                      limits = c(plot_y_min, plot_y_max),
                                      breaks = plotBreaks),
      squareRoot = scale_y_continuous(transform = "sqrt", limits = c(0, plot_y_max),
                                      breaks = plotBreaks,
                                      labels = scales::number_format(accuracy = 0.01)),
      normal     = scale_y_continuous(limits = c(0, plot_y_max), breaks = plotBreaks)
    )
  }

  return(viz)
}
