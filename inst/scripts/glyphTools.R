# glyphTools.R
#
# Utilities to convert ttf glyphs to polygons for plotting
#
# bs 2018-02
# ==============================================================================
#

if (!require(bezier, quietly = TRUE)) {
  install.packages("bezier")
  library(bezier)
}

# = Functions ================================
hex2bits <- function(xs) {
  # translate a raw byte into a vector of {0,1}
  bits <- numeric()
  a <- c(128, 64, 32, 16, 8, 4, 2, 1)
  for (x in xs) {
    x <- as.integer(x)
    bits <- c(bits, floor(x / a) %% 2)
  }
  return(bits)
}

bits2dec <- function(b) {
  # translate a vector of max 4 bytes to decimal
  a <- c(2147483648, 1073741824, 536870912, 268435456, 134217728, 67108864,
         33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 524288,
         262144, 131072, 65536, 32768, 16384, 8192, 4096, 2048, 1024, 512,
         256, 128, 64, 32, 16, 8, 4, 2, 1)
  b <- leftPad(b, 32)
  return(sum(a * b))
}

leftPad <- function(b, l) {
  # left-pad a vector b with 0 to length l
  return(c(rep(0, l - length(b)), b))
}

char2codepoint <- function(chr) {
  # given a charcter in UTF-8, calculate its codepoint in decimal
  # cf. https://en.wikipedia.org/wiki/UTF-8
  myRaw <- charToRaw(chr)
  nBytes <- length(myRaw)
  if (nBytes == 1) {
    myCodepoint <- bits2dec(myRaw)
  } else if (nBytes == 2) {
    myBytes <- hex2bits(myRaw)
    myCodepoint <- bits2dec(leftPad(myBytes[c(4:8, 11:16)], 16))
  } else if (nBytes == 3) {
    myBytes <- hex2bits(myRaw)
    myCodepoint <- bits2dec(leftPad(myBytes[c(5:8, 11:16, 19:24)], 24))
  } else if (nBytes == 4) {
    myBytes <- hex2bits(myRaw)
    myCodepoint <- bits2dec(leftPad(myBytes[c(5:8, 11:16, 19:24, 27:32)], 32))
  }
  return(myCodepoint)
}

rawPath2commands <- function(s) {
  # split a SVG path into its individual path commands
  s <- gsub("([A-Z])", ":\\1 ", s, perl = TRUE)
  x <- strsplit(s, ":")[[1]]
  x <- x[x != ""]
  return(strsplit(x, "\\s+", perl = TRUE))
}

reflectPoint <- function(p, ori) {
  # reflects p over ori
  return( ((p - ori) * -1) + ori)
}

svg2commands <- function(svg) {
  # process an svg file containing a glyph path into svg commands
  #
  # We regex the data string out of the <glyph /> element. If there is none,
  # we use the data string from the <missing-glyph /> element instead.
  #
  # return a list of paths containing the individual svg path commands
  #
  svg <- paste(svg, collapse = " ")
  x <- grep("<glyph ", svg)
  patt <- "<glyph .*?d=\\\"([^\\\"]+)\\\""
  m <- regexec(patt, svg)
  if (m[[1]][1] == -1) {
    patt <- "<missing-glyph .*?d=\\\"([^\\\"]+)\\\""
    m <- regexec(patt, svg)
  }
  d <- regmatches(svg, m)[[1]][2]

  x <- strsplit(d, "(?<=[zZ])", perl = TRUE)

  pathList <- list()
  for (i in seq_along(x[[1]])) {
    pathList[[i]] <- list()
    pathList[[i]]$rawPath <- x[[1]][i]
    pathCommands <- rawPath2commands(pathList[[i]]$rawPath)
    # transform T into Q
    for (j in seq_along(pathCommands)) {
      if (pathCommands[[j]][1] == "T") {
        cmdPrev <- pathCommands[[j - 1]]
        if (cmdPrev[1] == "Q") {
          controlPoint <- reflectPoint(p = as.numeric(cmdPrev[2:3]),
                                       ori = as.numeric(cmdPrev[4:5]))
          pathCommands[[j]] <- c("Q", as.character(controlPoint), pathCommands[[j]][2:3])
        }
      }
    }
    pathList[[i]]$commands <- pathCommands
  }

  return(pathList)
}


moveTo <- function(cmd) {
  # apply an SVG command and return the resulting xy coordinates
  stopifnot(cmd[1] == "M")
  cmd <- as.numeric(cmd[-1])
  stopifnot(all(! is.na(cmd)))
  stopifnot(length(cmd) >= 2)
  stopifnot((length(cmd) %% 2) == 0)
  # https://www.w3.org/TR/SVG/paths.html :
  #
  # Start a new sub-path at the given (x,y) coordinate. M (uppercase) indicates
  # that absolute coordinates will follow; m (lowercase) indicates that relative
  # coordinates will follow. If a moveto is followed by multiple pairs of
  # coordinates, the subsequent pairs are treated as implicit lineto commands.
  # Hence, implicit lineto commands will be relative if the moveto is relative,
  # and absolute if the moveto is absolute. If a relative moveto (m) appears as
  # the first element of the path, then it is treated as a pair of absolute
  # coordinates. In this case, subsequent pairs of coordinates are treated as
  # relative even though the initial moveto is interpreted as an absolute
  # moveto.
  #
  # ToDo: implement relative coordinates ("m")

  xy <- matrix(cmd, ncol = 2, byrow = TRUE)
  return(xy)
}

lineTo <- function(cmd) {
  # apply an SVG command and return the resulting xy coordinates
  stopifnot(cmd[1] == "L")
  cmd <- as.numeric(cmd[-1])
  stopifnot(all(! is.na(cmd)))
  stopifnot(length(cmd) >= 2)
  stopifnot((length(cmd) %% 2) == 0)
  # https://www.w3.org/TR/SVG/paths.html :
  #
  # Draw a line from the current point to the given (x,y) coordinate which
  # becomes the new current point. L (uppercase) indicates that absolute
  # coordinates will follow; l (lowercase) indicates that relative coordinates
  # will follow. A number of coordinates pairs may be specified to draw a
  # polyline.
  #
  # ToDo: implement relative coordinates ("l")

  xy <- matrix(cmd, ncol = 2, byrow = TRUE)
  return(xy)
}

horTo <- function(cmd, y) {
  # apply an SVG command and return the resulting xy coordinates
  stopifnot(cmd[1] == "H")
  cmd <- as.numeric(cmd[-1])
  stopifnot(all(! is.na(cmd)))
  stopifnot(length(cmd) >= 1)
  # https://www.w3.org/TR/SVG/paths.html :
  #
  # Draw a horizontal line from the current point (cpx, cpy) to (x, cpy). H
  # (uppercase) indicates that absolute coordinates will follow; h (lowercase)
  # indicates that relative coordinates will follow. Multiple x values can be
  # provided (although usually this doesn't make sense).
  #
  # ToDo: implement relative coordinates ("h")

  xy <- matrix(numeric(length(cmd) * 2), ncol = 2)
  xy[ , 1] <- cmd
  xy[ , 2] <- y
  return(xy)
}

verTo <- function(cmd, x) {
  # apply an SVG command and return the resulting xy coordinates
  stopifnot(cmd[1] == "V")
  cmd <- as.numeric(cmd[-1])
  stopifnot(all(! is.na(cmd)))
  stopifnot(length(cmd) >= 1)
  # https://www.w3.org/TR/SVG/paths.html :
  #
  # Draw a vertical line from the current point (cpx, cpy) to (cpx, y). V
  # (uppercase) indicates that absolute coordinates will follow; v (lowercase)
  # indicates that relative coordinates will follow. Multiple y values can be
  # provided (although usually this doesn't make sense).
  #
  # ToDo: implement relative coordinates ("v")

  xy <- matrix(numeric(length(cmd) * 2), ncol = 2)
  xy[ , 1] <- x
  xy[ , 2] <- cmd
  return(xy)
}

quadTo <- function(cmd, cP, N) {
  # apply an SVG command and return the resulting xy coordinates
  stopifnot(cmd[1] == "Q")
  cmd <- as.numeric(cmd[-1])
  stopifnot(all(! is.na(cmd)))
  stopifnot(length(cmd) >= 4)
  stopifnot((length(cmd) %% 4) == 0)
  # https://www.w3.org/TR/SVG/paths.html :
  #
  # Draws a quadratic Bézier curve from the current point to (x,y) using (x1,y1)
  # as the control point. Q (uppercase) indicates that absolute coordinates will
  # follow; q (lowercase) indicates that relative coordinates will follow.
  # Multiple sets of coordinates may be specified to draw a polybézier. At the
  # end of the command, the new current point becomes the final (x,y) coordinate
  # pair used in the polybézier.
  #
  # ToDo: implement relative coordinates ("l")
  #
  # cP: current Point
  # N:  number of points on Bezier

  pQ <- matrix(cmd, ncol = 4, byrow = TRUE)
  pQ <- cbind(c(cP[1], pQ[-nrow(pQ), 3]),
              c(cP[2], pQ[-nrow(pQ), 4]),
              pQ)

  xy <- matrix(c(numeric(), numeric()), ncol = 2)
  for (i in 1:nrow(pQ)) {
    bp <- bezier(t=seq(0, 1, length=N), p=matrix(pQ[i, ], ncol=2, byrow=TRUE))
    xy <- rbind(xy, bp[-1, ])
  }
  return(xy)
}


closePath <- function(cmd) {
  # apply an SVG command and return the resulting xy coordinates
  stopifnot(cmd[1] %in% c("Z", "z"))
  stopifnot(length(cmd) == 1)
  # https://www.w3.org/TR/SVG/paths.html :
  # Close the current subpath by drawing a straight line from the current point
  # to current subpath's initial point. Since the Z and z commands take no
  # parameters, they have an identical effect.
  #
  # ---
  # R polygons get closed implicitly, so return a null matrix only. This
  # function is a placeholder in case different approaches to path closing
  # would become necessary.

  xy <- matrix(c(numeric(), numeric()), ncol = 2)
  return(xy)
}


isCW <- function(xy) {
  # determine direction of rotation of a polygon
  #
  # https://stackoverflow.com/questions/1165647
  # /how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order
  #
  # This is required, because by convention CW paths are outside outlines and
  # CCW paths are holes

  N <- nrow(xy)
  xy <- cbind(xy, numeric(N)) # column for integrals
  colnames(xy)[3] <- "f"
  xy <- rbind(xy, xy[1, ])    # close point list
  for (i in 1:N) {
    xy[i, "f"] <- (xy[(i+1), "x"] - xy[i, "x"]) * (xy[(i+1), "y"] + xy[i, "y"])
  }
  twiceArea <- sum(xy[ , "f"])
  return(twiceArea >= 0)
}


commands2Points <- function(pL) {
  # iterate over a path list, execute the SVG commands, and collect the
  # resulting xy coordinate points.
  for (i in seq_along(pL)) {
    xy <- matrix(c(numeric(), numeric()), ncol = 2)
    colnames(xy) <- c("x", "y")
    for (j in seq_along(pL[[i]]$commands)) {
      cmd <- pL[[i]]$commands[[j]]
      if (cmd[1] %in% c("M")) {
        xy <- rbind(xy, moveTo(cmd))
      } else if (cmd[1] %in% c("L")) {
        xy <- rbind(xy, lineTo(cmd))
      } else if (cmd[1] %in% c("H")) {
        xy <- rbind(xy, horTo(cmd, y = xy[nrow(xy), "y"]))
      } else if (cmd[1] %in% c("V")) {
        xy <- rbind(xy, verTo(cmd, x = xy[nrow(xy), "x"]))
      } else if (cmd[1] %in% c("Q")) {
        xy <- rbind(xy, quadTo(cmd, cP = xy[nrow(xy), ], N = NBEZIERPOINTS))
      } else if (cmd[1] %in% c("T")) {
        # Bezier shorthand notation
        # https://www.w3.org/TR/SVG/paths.html: Draws a quadratic Bézier curve from
        # the current point to (x,y). The control point is assumed to be the
        # reflection of the control point on the previous command relative to the
        # current point. (If there is no previous command or if the previous command
        # was not a Q, q, T or t, assume the control point is coincident with the
        # current point.) T (uppercase) indicates that absolute coordinates will
        # follow; t (lowercase) indicates that relative coordinates will follow. At
        # the end of the command, the new current point becomes the final (x,y)
        # coordinate pair used in the polybézier.
        #
        # Note: There appears to be a bug in the ttf2svg conversion of the MS
        # Mincho Font, in that the curve of the first Bezier curve should start
        # at T (x, y), not at the preceding M (x, y). Since we are not drawing
        # the line, and this does not affect the closed polygon, it does not
        # matter. However, should we ever to decide to draw lines, we'll have to
        # fix that crap which is either introduced by batik-ttf2svg or already
        # by MS in their font.
        #
        # Fetch previous command
        cmdPrev <- pL[[i]]$commands[[j-1]]
        # Fetch current point
        currP <- xy[nrow(xy), ]
        # Expand command to single-point T commands
        cmd <- matrix(cmd[-1], ncol = 2)
        cmd <- cbind(rep("T", nrow(cmd)), cmd)
        # process each row
        for (k in 1:nrow(cmd)) {
          if (cmdPrev[1] %in% c("Q")) {
            controlPoint <- reflectPoint(p   = as.numeric(cmdPrev[2:3]),
                                         ori = currP)
          } else {
            controlPoint <- currP
          }
          Q <- c("Q", as.character(controlPoint), cmd[k, 2:3])
          xy <- rbind(xy, quadTo(Q, cP = currP, N = NBEZIERPOINTS))
          cmdPrev <- Q
          currP <- xy[nrow(xy), ]
        }
      } else if (cmd[1] %in% c("Z", "z")) {
        xy <- rbind(xy, closePath(cmd))
      } else {
        stop(sprintf("SVG path command \"%s\" is not implemented.",
                     paste0(cmd, collapse = " ")))
      } # end if/else SVG path command parsing
    } # end for (j in seq_along(pL[[i]]$commands))
    pL[[i]]$xy <- xy
    pL[[i]]$CW <- isCW(xy)
  } # for (i in seq_along(pL))

  return(pL)
}


bBox <- function(paths) {
  # calculate the bounding box of a path list. This is the bounding box of
  # the whole glyph, not just one of its subpaths.
  xMin <-  Inf
  xMax <- -Inf
  yMin <-  Inf
  yMax <- -Inf
  for (i in seq_along(paths)) {
    xMin <- min(xMin, paths[[i]]$xy[ , "x"])
    xMax <- max(xMax, paths[[i]]$xy[ , "x"])
    yMin <- min(yMin, paths[[i]]$xy[ , "y"])
    yMax <- max(yMax, paths[[i]]$xy[ , "y"])
  }
  return(c(xMin, yMin, xMax, yMax))
}



makeGlyph <- function(c, font = GLYPHFONT, svgFile = tempfile()) {
  # create a glyph list, given a UTF-8 character
  glyph <- list()

  glyph$char <- c
  glyph$font <- font
  glyph$codepoint <- char2codepoint(glyph$char)

  # codepoint to svg
  # https://xmlgraphics.apache.org/batik/tools/font-converter.html
  myCommand <- sprintf("batik-ttf2svg %s -l %d -h %d -id xSVG -o %s",
                       glyph$font,
                       glyph$codepoint,
                       glyph$codepoint,
                       svgFile)
  system(command = myCommand)
  mySVG <- readLines(svgFile)
  glyph$paths <- svg2commands(mySVG)
  glyph$paths <- commands2Points(glyph$paths)
  glyph$bbox <- bBox(glyph$paths)

  return(glyph)
}


glyphPlot <- function(glyph, fill = "#000000", bg = par("bg"), lty = "blank") {
  # plot one glyph into its native bounding box.
  #
  # We first plot an empty frame, then the outlines as filled polygons,
  # finally the holes as polygons filled with the background colour.
  #
  # ToDo: There's an annoying bug in either batik or the MS-Font which
  #       we will need to patch at some point to draw clean outline lines.
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


# [END]
