# === Parameters =============

NBEZIERPOINTS <- 20 # number of points on a flattened Bezier segment
GLYPHFONT <- "~/MS-Mincho.ttf"

# === Tools ==================

source("./inst/scripts/glyphTools.R")

getFrequencies <- function(column, gapCharacter='-') {
  strippedGaps <- column[column != gapCharacter]
  return(table(strippedGaps) / length(strippedGaps))
}

calculateShannonEntropy <- function(freqs) {
  return(-sum(freqs * log2(freqs)))
}

smallSampleCorrection <- function(numSamples, aminoAcids=FALSE) {
  if (aminoAcids) {
    s <- 20
  } else {
    s <- 4
  }
  return(s/(log(2) * 2 * numSamples))
}

calcInformation <- function(freqs, correction = 0, aminoAcids=FALSE) {
  if (aminoAcids) {
    b <- 20
  } else {
    b <- 4
  }
  entropy <- calculateShannonEntropy(freqs)
  return(log2(b) - (entropy - correction))
}

calcHeights <- function(freqs, information) {
  return(freqs * information)
}

maxInformation <- function(aminoAcids=FALSE) {
  if (aminoAcids) {
    b <- 20
  } else {
    b <- 4
  }
  return(log(b))
}

myGlyphs[["A"]] <- makeGlyph("A")
myGlyphs[["T"]] <- makeGlyph("T")
myGlyphs[["C"]] <- makeGlyph("C")
myGlyphs[["G"]] <- makeGlyph("G")

ccooll <- c('T', 'T', 'A', 'T', '-', 'G', 'C', 'A')
freqs <- getFrequencies(ccooll)
correction <- smallSampleCorrection(length(ccooll) - 1)
info <- calcInformation(freqs, correction)
heights <- calcHeights(freqs, info)
sorted <- heights[order(heights)]

currBottom <- 0
maxInfo <- maxInformation(4)
for (i in 1:length(sorted)) {
  base <- names(sorted)[i]
  height <- sorted[i]
  plotHeight <- height / maxInfo
  glyphPlot(myGlyphs[[base]], bBox(currBottom, 0.0, height, 1.0))
  currBottom <- height
}


ccooll <- c('A', 'C', 'C', '-', '-', 'G', 'C', 'A')
freqs <- getFrequencies(ccooll)
correction <- smallSampleCorrection(length(ccooll) - 1)
info <- calcInformation(freqs, correction)
heights <- calcHeights(freqs, info)
sorted <- heights[order(heights)]
