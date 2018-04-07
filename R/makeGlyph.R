# makeGlyph.R

#' \code{makeGlyph} convert a character into a list of paths to draw.
#'
#' Details.
#' @param c a character
#' @param fontPath the path to the ttf to use
#' @param svgFile the path to store the temporary converted ttf to svg file
#' @param numBezierPoints number of bezier points to use when generating curved paths
#' @return a glyph
#' @export
makeGlyph <- function(c, fontPath = system.file("extdata/notosans", "NotoSans-Regular.ttf", package = "shi"),
                      svgFile = tempfile(), numBezierPoints = 20) {
  # create a glyph list, given a UTF-8 character
  glyph <- list()

  glyph$char <- c
  glyph$font <- fontPath
  glyph$codepoint <- char2codepoint(glyph$char)

  # codepoint to svg
  # https://xmlgraphics.apache.org/batik/tools/font-converter.html
  ttf2svgLocation <- system.file("extdata/batik", "batik-ttf2svg-1.9.jar", package = "shi")
  myCommand <- sprintf("java -jar %s %s -l %d -h %d -id xSVG -o %s",
                       ttf2svgLocation,
                       glyph$font,
                       glyph$codepoint,
                       glyph$codepoint,

                       svgFile)
  system(command = myCommand)
  mySVG <- readLines(svgFile)
  glyph$paths <- svg2commands(mySVG)
  glyph$paths <- commands2Points(glyph$paths, numBezierPoints)
  glyph$bbox <- bBox(glyph$paths)

  return(glyph)
}
