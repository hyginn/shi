makeScalingClosure <- function(oldBox, newBox) {
  # Teturns a function that scales glyph path xy coordinates from a glyph
  # bounding box of oldBox to a glyph bounding box of newBox.
  #
  # ToDo: refactor this to proper generalized 2D transformations with
  #       homogeneous coordinates.

  f <- function(xy) {
    # translate lower left of oldBox to origin
    xy[ , "x"] <- xy[ , "x"] - oldBox[1]
    xy[ , "y"] <- xy[ , "y"] - oldBox[2]

    # scale x and y
    scaleX <- (newBox[3] - newBox[1]) / (oldBox[3] - oldBox[1])
    scaleY <- (newBox[4] - newBox[2]) / (oldBox[4] - oldBox[2])
    xy[ , "x"] <- xy[ , "x"] * scaleX
    xy[ , "y"] <- xy[ , "y"] * scaleY

    # translate lower left to target
    xy[ , "x"] <- xy[ , "x"] + newBox[1]
    xy[ , "y"] <- xy[ , "y"] + newBox[2]

    return(xy)
  }
  return(f)
}


glyphPoly <- function(glyph,
                      bbox = c(0, 0, 1, 1),
                      fill = "#000000",
                      bg = par("bg"),
                      lty = "blank") {
  # plot a glyph as a polygon into the bbox target bounding box

  f <- makeScalingClosure(glyph$bbox, bbox)

  # shapes
  for (i in seq_along(glyph$paths)) {
    if (glyph$paths[[i]]$CW == TRUE) {
      polygon(f(glyph$paths[[i]]$xy), col = fill, lty = lty)
    }
  }

  # holes
  for (i in seq_along(glyph$paths)) {
    if (glyph$paths[[i]]$CW == FALSE) {
      polygon(f(glyph$paths[[i]]$xy), col = bg, lty = lty)
    }
  }
}