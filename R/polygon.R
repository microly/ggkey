#' A polygon key glyphs for legends of size aesthetics
#'
#' @param width A numeric to control the width of keys,
#'   which should be between 0 and 1.
#' @param height A numeric to control the height of keys,
#'   which should be between 0 and 1.
#'
#' @return A Key glyphs function.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' library(ggplot2)
#'
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"),
#'                  quiet = TRUE)
#' nc2 <- nc %>% arrange(desc(AREA)) %>% slice(1:2)
#'
#' ggplot(nc2) +
#'     geom_sf(aes(size = AREA),
#'             colour = "red") +
#'     scale_size(breaks = c(0.24, 0.241)) +
#'     theme(legend.key.size = unit(0.15, "npc"))
#'
#' ggplot(nc2) +
#'     geom_sf(aes(size = AREA),
#'             colour = "red",
#'            key_glyph = draw_key_polygon_size()) +
#'     scale_size(breaks = c(0.24, 0.241)) +
#'     theme(legend.key.size = unit(0.15, "npc"))
#'
#' ggplot(nc2) +
#'     geom_sf(aes(size = AREA),
#'             colour = "red",
#'             key_glyph = draw_key_polygon_size(0.9, 0.3)) +
#'     scale_size(breaks = c(0.24, 0.241)) +
#'     theme(legend.key.size = unit(0.15, "npc"))
#'
draw_key_polygon_size <- function(width = 0.8, height = 0.8) {

    function(data, params, size) {
        if (is.null(data$size)) {
            data$size <- 0.5
        }

        lwd <- min(data$size, min(size) / 4)

        rectGrob(
            width = unit(width, "npc"),
            height = unit(height, "npc"),
            gp = gpar(
                col = data$colour %||% NA,
                fill = alpha(data$fill %||% "grey90", data$alpha),
                lty = data$linetype %||% 1,
                lwd = lwd * .pt,
                linejoin = params$linejoin %||% "mitre",
                lineend = if (identical(params$linejoin, "round")) "round" else "square"
            ))
    }
}
