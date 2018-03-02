# UTF2glyph.R
#
# Sample code: create and transform glyphs from a ttf file
#
# bs 2018-02
#
# Note: this requires installation of Apache batik-ttf2svg
# https://xmlgraphics.apache.org/batik/tools/font-converter.html
#

# === Parameters =============

NBEZIERPOINTS <- 20 # number of points on a flattened Bezier segment
GLYPHFONT <- "~/MS-Mincho.ttf"

# === Tools ==================

source("./inst/scripts/glyphTools.R")

# === Examples ===============

myGlyphs <- list()

myGlyphs[["A"]] <- makeGlyph("A")   # Latin A
myGlyphs[["a"]] <- makeGlyph("a")   # Latin a
myGlyphs[["α"]] <- makeGlyph("α")   # Greek alpha
myGlyphs[["Å"]] <- makeGlyph("Å")   # Scandinavian A ring
myGlyphs[["@"]] <- makeGlyph("@")   # at
myGlyphs[["☥"]] <- makeGlyph("☥")   # Egyptian Ankh
myGlyphs[["あ"]] <- makeGlyph("あ") # Japenese Hiragana "a"
myGlyphs[["啊"]] <- makeGlyph("啊") # Chinese sound "Ah"

glyphPlot(myGlyphs[["A"]], fill = "seagreen")
glyphPlot(myGlyphs[["a"]], fill = "skyblue")
glyphPlot(myGlyphs[["α"]], fill = "tomato")
glyphPlot(myGlyphs[["Å"]], fill = "thistle")
glyphPlot(myGlyphs[["@"]], fill = "violetred")
glyphPlot(myGlyphs[["☥"]], fill = "maroon")
glyphPlot(myGlyphs[["あ"]], fill = "firebrick")
glyphPlot(myGlyphs[["啊"]], fill = "goldenrod")


# plot as polygon into given bounding box
myCols <- colorRampPalette(c("#C27E7E", "#816EBA", "#758AC9", "#82C9B6"))(8)

plot(0, 0,
     xlim = c(0, 3),
     ylim = c(0, 5),
     type = "n")

# Note that the "a" is flipped vertically, and the "@" is flipped horizontally
# by appropriate definition of the target bounding box
glyphPoly(myGlyphs[["A"]], bbox = c(0.5, 0.0, 0.8, 1.5), fill = myCols[1])
glyphPoly(myGlyphs[["a"]], bbox = c(0.8, 1.5, 1.1, 0.0), fill = myCols[2])
glyphPoly(myGlyphs[["☥"]], bbox = c(1.1, 0.0, 1.5, 1.5), fill = myCols[3])
glyphPoly(myGlyphs[["α"]], bbox = c(0.5, 1.5, 1.5, 2.2), fill = myCols[4])
glyphPoly(myGlyphs[["Å"]], bbox = c(0.5, 2.2, 1.5, 2.7), fill = myCols[5])
glyphPoly(myGlyphs[["@"]], bbox = c(1.5, 2.7, 0.5, 3.1), fill = myCols[6])
glyphPoly(myGlyphs[["あ"]], bbox = c(0.5, 3.1, 1.5, 4.4), fill = myCols[7])
glyphPoly(myGlyphs[["啊"]], bbox = c(0.5, 4.4, 1.5, 5.0), fill = myCols[8])

# [END]
