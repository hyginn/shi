rawPath2commands <- function(s) {
  # split a SVG path into its individual path commands
  s <- gsub("([A-Z])", ":\\1 ", s, perl = TRUE)
  x <- strsplit(s, ":")[[1]]
  x <- x[x != ""]
  return(strsplit(x, "\\s+", perl = TRUE))
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
    pathList[[i]]$commands <- pathCommands
  }
  return(pathList)
}