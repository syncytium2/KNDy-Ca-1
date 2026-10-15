plot_selected_runs_treatments <- function(df_in_0, run_type, run_ids, select_treatments,
                        groups = c("DI", "OVX", "MALE", "ORX"),
                        slice_colors,
                        freq_overall_min = 0,   freq_overall_max = 0.7,
                        amp_overall_min  = 0.003, amp_overall_max = 2,
                        width_overall_min = 0.1, width_overall_max = 30,
                        td_sigma     = 0.003,
                        quasi_method = "smiley",
                        plotType     = "quasirandom",
                        replace_str  = NULL) {

  results <- list()

  for (s in select_treatments) {
    treats    <- c("baseline", s)
    i_run     <- 0
    runs_slices <- NULL
    runs_groups <- NULL
    run_plots   <- list()

    for (data_run in run_ids) {
      data_run2 <- if (!is.null(replace_str)) {
        str_replace(data_run, replace_str[1], replace_str[2])
      } else {
        data_run
      }

      i_run <- i_run + 1

      df   <- df_in_0[df_in_0$run_id == data_run, ]
      ROIs <- df[round(df$duration) >= 12, ]
      ROIs <- ROIs[ROIs$treatment_idx <= 2, ]

      select_only     <- ROIs[ROIs$treatment == s & ROIs$treatment_idx == 2, ]
      select_slice_ids <- unique(select_only$slice_id)
      select_ROIs     <- ROIs[ROIs$slice_id %in% select_slice_ids, ]

      select_ROIs$treatment <- factor(select_ROIs$treatment, levels = treats)
      select_ROIs$group_id  <- factor(select_ROIs$group_id,  levels = groups)
      select_ROIs$slice_id  <- factor(select_ROIs$slice_id)

      select_ROIs <- select_ROIs %>%
        group_by(group_id) %>%
        mutate(local_slice = as.factor(as.numeric(as.factor(slice_idx)))) %>%
        ungroup()

      select_slices <- select_ROIs %>%
        group_by(group_id, slice_id, local_slice, treatment) %>%
        summarize(
          duration           = unique(duration),
          num_ROIs           = n(),
          num_ROIs_zero_freq = sum(freq == 0),
          num_events         = sum(n_events),
          s_freq_max         = max(freq,        na.rm = TRUE),
          s_freq_min         = min(freq,        na.rm = TRUE),
          s_freq_mean        = mean(freq,       na.rm = TRUE),
          s_freq_median      = median(freq,     na.rm = TRUE),
          s_amp_max          = max(amp_median,  na.rm = TRUE),
          s_amp_min          = min(amp_median,  na.rm = TRUE),
          s_amp_mean         = mean(amp_median, na.rm = TRUE),
          s_amp_median       = median(amp_median, na.rm = TRUE),
          s_width_max        = max(width_median,  na.rm = TRUE),
          s_width_min        = min(width_median,  na.rm = TRUE),
          s_width_mean       = mean(width_median, na.rm = TRUE),
          s_width_median     = median(width_median, na.rm = TRUE),
          .groups = "drop"
        )

      select_slices$run_id <- data_run2
      select_slices <- select_slices |> relocate(run_id)

      select_groups <- select_slices %>%
        group_by(group_id, treatment) %>%
        summarize(
          g_freq_mean  = mean(s_freq_mean,  na.rm = TRUE),
          g_freq_se    = sd(s_freq_mean,    na.rm = TRUE) / sqrt(n()),
          g_amp_mean   = mean(s_amp_mean,   na.rm = TRUE),
          g_amp_se     = sd(s_amp_mean,     na.rm = TRUE) / sqrt(n()),
          g_width_mean = mean(s_width_mean, na.rm = TRUE),
          g_width_se   = sd(s_width_mean,   na.rm = TRUE) / sqrt(n()),
          num_slices   = n(),
          num_events   = sum(num_events),
          .groups = "drop"
        )

      select_groups$run_id <- data_run2
      select_groups <- select_groups |> relocate(run_id)

      if (i_run == 1) {
        runs_slices <- select_slices
        runs_groups <- select_groups
      } else {
        runs_slices <- rbind(runs_slices, select_slices)
        runs_groups <- rbind(runs_groups, select_groups)
      }

      parameters <- c("frequency", "amplitude", "width")

      plots <- lapply(parameters, function(param) {
        switch(param,
          frequency = {
            ylabel            <- "frequency (Hz)"
            scaleYtype        <- "pseudolog"
            hide_facet_titles <- FALSE
            hide_xlabels      <- TRUE
            plot_min          <- freq_overall_min
            plot_max          <- freq_overall_max
            forceYticks       <- c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)
            select_slice_summary <- data.frame(
              group_id    = select_slices$group_id,
              local_slice = select_slices$local_slice,
              treatment   = select_slices$treatment,
              s_mean      = select_slices$s_freq_mean
            )
            select_group_summary <- data.frame(
              group_id  = select_groups$group_id,
              treatment = select_groups$treatment,
              g_mean    = select_groups$g_freq_mean,
              g_se      = select_groups$g_freq_se
            )
          },
          amplitude = {
            ylabel            <- "amplitude (ΔF/F0)"
            scaleYtype        <- "log"
            hide_facet_titles <- TRUE
            hide_xlabels      <- TRUE
            plot_min          <- amp_overall_min
            plot_max          <- amp_overall_max
            forceYticks       <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1)
            select_slice_summary <- data.frame(
              group_id    = select_slices$group_id,
              local_slice = select_slices$local_slice,
              treatment   = select_slices$treatment,
              s_mean      = select_slices$s_amp_mean
            )
            select_group_summary <- data.frame(
              group_id  = select_groups$group_id,
              treatment = select_groups$treatment,
              g_mean    = select_groups$g_amp_mean,
              g_se      = select_groups$g_amp_se
            )
          },
          width = {
            ylabel            <- "width (seconds)"
            scaleYtype        <- "log"
            hide_facet_titles <- TRUE
            hide_xlabels      <- FALSE
            plot_min          <- width_overall_min
            plot_max          <- width_overall_max
            forceYticks       <- c(0.3, 1, 3, 10)
            select_slice_summary <- data.frame(
              group_id    = select_slices$group_id,
              local_slice = select_slices$local_slice,
              treatment   = select_slices$treatment,
              s_mean      = select_slices$s_width_mean
            )
            select_group_summary <- data.frame(
              group_id  = select_groups$group_id,
              treatment = select_groups$treatment,
              g_mean    = select_groups$g_width_mean,
              g_se      = select_groups$g_width_se
            )
          }
        ) # end switch

        before_after_v6(
          select_ROIs, select_slice_summary, select_group_summary,
          switch(param,
            frequency = select_ROIs$freq,
            amplitude = select_ROIs$amp_median,
            width     = select_ROIs$width_median
          ),
          ylabel, slice_colors, plotType, scaleYtype,
          hide_facet_titles, hide_xlabels,
          plot_y_min = plot_min, plot_y_max = plot_max,
          forceYticks, td_sigma, quasi_method
        )
      })
      names(plots) <- parameters

      viz_grid <- ggarrange(plots[["frequency"]], plots[["amplitude"]], plots[["width"]],
                            margin(0, 0, 0, 1, "cm"), ncol = 1)

      title_label <- paste0(run_type, "_", data_run2, "_pseudolog_sigma_", td_sigma)
      title <- ggdraw() +
        draw_label(title_label, fontface = "bold", x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 1, "cm"))

      viz_grid <- plot_grid(title, viz_grid, ncol = 1, rel_heights = c(0.1, 1))

      run_plots[[data_run2]] <- list(
        plots  = plots,
        grid   = viz_grid,
        slices = select_slices,
        groups = select_groups
      )
    } # end run loop

    runs_groups$treatment <- factor(runs_groups$treatment)
    n_events_plot <- ggplot(runs_groups, aes(x = treatment, y = num_events, color = run_id)) +
      geom_point() +
      facet_grid(. ~ group_id, scales = "free_x", space = "free_x")

    results[[s]] <- list(
      runs          = run_plots,
      runs_slices   = runs_slices,
      runs_groups   = runs_groups,
      n_events_plot = n_events_plot
    )
  } # end treatment loop

  results
}
