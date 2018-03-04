makeGlyph <- function(c, font = GLYPHFONT, svgFile = tempfile(), numBezierPoints = 20) {
  # create a glyph list, given a UTF-8 character
  glyph <- list()

  glyph$char <- c
  glyph$font <- font
  glyph$codepoint <- char2codepoint(glyph$char)

  # codepoint to svg
  # https://xmlgraphics.apache.org/batik/tools/font-converter.html
  javaExec <- "java -jar /home/adrl/Downloads/batik/batik-1.9/batik-ttf2svg-1.9.jar"
  myCommand <- sprintf("%s %s -l %d -h %d -id xSVG -o %s",
                       javaExec,
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