# edited from geom_errorbar
# https://github.com/tidyverse/ggplot2/blob/main/R/geom-errorbar.R
# https://stackoverflow.com/questions/27585776/error-bars-for-barplot-only-in-one-direction

#' @export
#' @rdname geom_linerange
geom_meanbar <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMeanbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomMeanbar <- ggproto("GeomMeanbar", Geom,
                        default_aes = aes(colour = "black", linewidth = 0.9, linetype = 1, width = 0.6,
                                          alpha = 1),
                        
                        draw_key = draw_key_path,
                        
                        required_aes = c("x", "y"),
                        
                        # requires ymin/ymax
                        # setup_params = function(data, params) {
                        #   GeomLinerange$setup_params(data, params)
                        # },
                       
                       setup_params = function(data, params) {
                         # Just return params without alteration
                         params
                       },
                        
                        extra_params = c("na.rm", "orientation"),
                        
                        setup_data = function(data, params) {
                          data$flipped_aes <- params$flipped_aes
                          data <- flip_data(data, params$flipped_aes)
                          data$width <- data$width %||%
                            params$width %||% (resolution(data$x, FALSE) * 0.9)
                          data <- transform(data,
                                            xmin = x - width / 2, xmax = x + width / 2, width = NULL
                          )
                          flip_data(data, params$flipped_aes)
                        },
                       
                       # this works, but may have some oddities umgpt
                       
                       draw_panel = function(self, data, panel_params, coord, lineend = "butt", width = NULL, flipped_aes = FALSE) {
                         # Check if check_linewidth exists in ggplot2 namespace
                         if (exists("check_linewidth", where = getNamespace("ggplot2"), inherits = FALSE)) {
                           data <- ggplot2:::check_linewidth(data, snake_class(self))
                         } else {
                           # print("Fallback")
                           # Fallback: assume linewidth is already valid
                           if (is.null(data$linewidth)) {
                             data$linewidth <- 0.5  # default fallback
                           }
                         }
                         
                         data <- flip_data(data, flipped_aes)
                         
                         if (!is.null(width)) {
                           data$width <- width
                         } else {
                           data$width <- data$width %||% (resolution(data$x, FALSE) * 0.9)
                         }
                         
                         coords <- coord$transform(data, panel_params)
                         
                         grid::segmentsGrob(
                           x0 = unit(coords$xmin, "native"),
                           y0 = unit(coords$y, "native"),
                           x1 = unit(coords$xmax, "native"),
                           y1 = unit(coords$y, "native"),
                           gp = grid::gpar(
                             col = coords$colour,
                             lwd = coords$linewidth * .pt,
                             lty = coords$linetype,
                             alpha = coords$alpha,
                             lineend = lineend
                           )
                         )
                       },
                        
                        rename_size = TRUE
)
