glyphPlot <- function(glyph, fill = "#000000", bg = par("bg"), lty = "blank") {
  # plot one glyph into its native bounding box.
  #
  # We first plot an empty frame, then the outlines as filled polygons,
  # finally the holes as polygons filled with the background colour.
  plot(0, 0,
       xlim = glyph$bbox[c(1, 3)],
       ylim = glyph$bbox[c(2, 4)],
       type = "n")

  # shapes
  for (i in seq_along(glyph$paths)) {
    if (glyph$paths[[i]]$CW == TRUE) {
      polygon(glyph$paths[[i]]$xy, col = fill, lty = lty)
    }
  }

  # holes
  for (i in seq_along(glyph$paths)) {
    if (glyph$paths[[i]]$CW == FALSE) {
      polygon(glyph$paths[[i]]$xy, col = bg, lty = lty)
    }
  }
}