# glyphPlot.R

#' \code{glyphPlot} plot a single glyph.
#'
#' Details.
#' @param glyph a glyph set to plot.
#' @param fill color to fill in glyph.
#' @param bg color of the background.
#' @param lty line type to use.
#' @return None.
#' @export
glyphPlot <- function(glyph, fill = "#000000", bg = graphics::par("bg"), lty = "blank") {
  # plot one glyph into its native bounding box.
  #
  # We first plot an empty frame, then the outlines as filled polygons,
  # finally the holes as polygons filled with the background colour.
  graphics::plot(0, 0,
       xlim = glyph$bbox[c(1, 3)],
       ylim = glyph$bbox[c(2, 4)],
       type = "n")

  # shapes
  for (i in seq_along(glyph$paths)) {
    if (glyph$paths[[i]]$CW == TRUE) {
      graphics::polygon(glyph$paths[[i]]$xy, col = fill, lty = lty)
    }
  }

  # holes
  for (i in seq_along(glyph$paths)) {
    if (glyph$paths[[i]]$CW == FALSE) {
      graphics::polygon(glyph$paths[[i]]$xy, col = bg, lty = lty)
    }
  }
}