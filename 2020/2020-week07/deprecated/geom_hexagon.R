library(grid)

GeomHexagon <- ggproto("GeomHexagon", Stat,
                       compute_group = function(data, scales) {
                         data[chull(data$x, data$y), , drop = FALSE]
                         },
                       
                       required_aes = c("x", "y")
)
                                    

geom_hexagon <- function(mapping = NULL, data = NULL, geom = "polygon", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = GeomHexagon, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}