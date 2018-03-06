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
    bp <- bezier::bezier(t=seq(0, 1, length=N), p=matrix(pQ[i, ], ncol=2, byrow=TRUE))
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


reflectPoint <- function(p, ori) {
  # reflects p over ori
  return( ((p - ori) * -1) + ori)
}

commands2Points <- function(pL, numBezierPoints = 20) {
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
        xy <- rbind(xy, quadTo(cmd, cP = xy[nrow(xy), ], N = numBezierPoints))
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
          xy <- rbind(xy, quadTo(Q, cP = currP, N = numBezierPoints))
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