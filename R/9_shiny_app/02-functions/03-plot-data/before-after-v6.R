before_after_v6 <- function( df, slice_summary, group_summary, yresult, ylabel,
                             slice_colors,
                             plotType, scaleYtype,
                             hide_facet_titles, hide_xlabels,
                             plot_y_min, plot_y_max,
                             forceYticks=NULL, td_sigma=NULL, quasi_method ) {

  # force_Y_ticks <- missing or NULL, use min/max
  # force_Y_ticks length > 1, use provided ticks, must be ascending, non-zero for log

  # data.frame variables
  # local_slice
  # s_
  # g_

#  ylabel2 <- paste( ylabel, scaleYtype )
  ylabel2 <- paste( ylabel, scaleYtype )
ylabel2 <- ylabel # temp fix for label too far


  summary_color <- 'black'
  small_dot_size <-0.75
  large_dot_size <-1.5
  line_size <- 0.25
  font_choice <- "Arial"
  font_size <- 11
  left_margin <- 0.5
  top_margin <- 0.5
  swarm_width <- 0.3
  nudge <- 0.35

  viz <- ggplot(df, aes(x = treatment, y = yresult, color = local_slice))
 xgroups<-unique(df$treatment)

   super_max = max( yresult, na.rm=T )
  if( super_max > plot_y_max ) {
    super_maxes <- yresult[yresult>plot_y_max]
    n_super_max <- length(super_maxes)
    annot <- paste("  mx:",num2str(super_max),"n:",num2str(n_super_max,0),"up:",num2str(plot_y_max,1))
    annot
    viz <- viz + annotate("text",x="baseline",y=plot_y_max,label=annot,size=1.5)
  }
  viz <- viz + switch( plotType,
                       jitter = geom_jitter( height=0, width = 0.35, alpha = 0.5,
                                             size = small_dot_size, stroke=0, na.rm=T ),
                       beeswarm = geom_beeswarm( stroke = 0, cex=3, size = small_dot_size, alpha = 0.5,  na.rm=T ),
                       quasirandom = geom_quasirandom( method = quasi_method, alpha = 0.5,
                                                      size = small_dot_size, stroke=0,
                                                      width = swarm_width, na.rm=T)
                     )
  # quasirandom methods: "quasirandom" (default), "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey", "tukeyDense"
  viz <- viz +
    geom_line(
      data = slice_summary,
      aes(x = treatment, y = s_mean, group = local_slice, color = local_slice),
      alpha = 0.7, size = line_size,
      position = position_nudge(x=c(nudge,-nudge))
    ) +
    geom_point(
      data = slice_summary,
      aes(x = treatment, y = s_mean, fill = local_slice),
      shape = 21, color = "black", size = large_dot_size,
      stroke=NA,
      position = position_nudge(x=c(nudge,-nudge)),
            na.rm = T
    ) +
    geom_errorbar(
      data = group_summary,
      aes(
        x = treatment,
        ymin = g_ymin,
        ymax = g_ymax
      ),
      color = summary_color,
      width = 0.15,
      size = line_size,
      #position = position_nudge(x=c(nudge,-nudge)),
      inherit.aes = FALSE
    ) +
    geom_point(
      data = group_summary,
      aes(x = treatment, y = g_mean),
      color = summary_color,
      size = large_dot_size,
      stroke=0,
      inherit.aes = FALSE,
      #position = position_nudge(x=c(nudge,-nudge)),
      na.rm = T
    ) +
    geom_line(
      data = group_summary,
      aes(x = treatment, y = g_mean, group=group_id),
      color = summary_color,
      size = line_size,
      #position = position_nudge(x=c(nudge,-nudge)),
      inherit.aes = FALSE
    ) +
    facet_grid( . ~ group_id, scales = "free_x", space = "free_x") +
    scale_color_manual( values = slice_colors) +
    scale_fill_manual( values = slice_colors) +
    labs( x = NULL, y = ylabel2 ) +
    theme_bw( base_size = font_size, base_family = font_choice )


  if(hide_facet_titles) {
    viz <- viz + theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none", # top",
      strip.text.x = element_blank(), # element_text( size = 12 ), # face = "bold",
      strip.background = element_blank(), #, element_rect( colour = "NA", fill = "NA" )
      axis.text.x = switch(hide_xlabels, T=element_blank(), F=NULL)
      ,plot.margin = margin( top_margin, 0, 0, left_margin, unit='cm')
    ) }
  else {
    viz <- viz + theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text.x = element_text( size = font_size, family = font_choice ), # face = "bold",
      strip.background = element_rect( colour = "NA", fill = "NA" ),
      axis.text.x = switch(hide_xlabels, T=element_blank(), F=NULL)
      , plot.margin = margin( top_margin, 0, 0, left_margin, unit='cm')
    ) }

  nbreaks=3
  if(is.null(td_sigma)) {
    td_sigma <- 0.001 # 0.0005 squishes important part # 10^(-nbreaks)
  }

  # breaks <- 10^-(nbreaks:1)
  # breaks <- c(0,breaks)
  breaks <- c(0,0.001,0.003,0.01,0.03,0.1,0.3,1,1.3)
  if(is.null(forceYticks)) {
    print("forceYticks is null...")
    viz <- viz + switch( scaleYtype,
                         log = scale_y_log10( limits=c( plot_y_min, plot_y_max) ),
                         pseudolog = scale_y_continuous( trans = pseudo_log_trans(sigma = td_sigma, base = 10),
                                                         breaks=breaks,
                                                         limits=c( plot_y_min, plot_y_max)),
                         squareRoot = scale_y_continuous(trans = "sqrt", limits=c( 0, plot_y_max),labels = scales::number_format(accuracy = 0.01)),
                         normal = scale_y_continuous( limits=c( 0, plot_y_max ) )
    )

  } else {
    #print("forceYticks is NOT null")
    if( length(forceYticks)>1 ) {
      plotBreaks <- forceYticks
      #print("using forceYticks")
    } else {
      plotBreaks <-pretty( c(plot_y_min,plot_y_max), 7 )
      print("guessing what forceYticks should be...")
    }
    viz <- viz + switch( scaleYtype,
                         log =  scale_y_log10( limits=c( plot_y_min, plot_y_max), breaks=plotBreaks ),
                         pseudolog = scale_y_continuous( trans = pseudo_log_trans(sigma = td_sigma, base = 10),
                                                         limits=c( plot_y_min, plot_y_max),
                                                         breaks=plotBreaks),
                         squareRoot = scale_y_continuous(transform = "sqrt", limits=c( 0, plot_y_max), breaks=plotBreaks, labels = scales::number_format(accuracy = 0.01) ),
                         normal = scale_y_continuous( limits=c( 0, plot_y_max ), breaks = plotBreaks )
    ) }

  # option to hide title
  #viz <- viz + ggtitle(data_set_name)
  return(viz)
}
