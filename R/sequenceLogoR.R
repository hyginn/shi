# sequenceLogoR.R

# something something documentation

# refactor this out later

# === Tools ==================

# source("./inst/scripts/glyphTools.R")
# source("./inst/scripts/sequenceLogos.R")
#
# if (! require(msa, quietly=TRUE)) {
#   if (! exists("biocLite")) {
#     source("https://bioconductor.org/biocLite.R")
#   }
#   biocLite("msa")
#   library(msa)
# }
#
# mySequenceFile <- system.file("examples", "exampleAA.fasta", package="msa")
# mySequences <- readAAStringSet(mySequenceFile)
# mySequences
#
# myFirstAlignment <- msa(mySequences, "Muscle")
#
# m <- as.character(myFirstAlignment)
# z <- AAStringSet(m)

sequenceLogoR <- function(alignment, settingsMap, isAminoAcid = FALSE, start = 1, end = 0, gapChracter = '-', calcCorrection = FALSE) {
  # generate glyphs
  glyphs <- list()
  colors <- list()
  for (item in settingsMap) {
    base <- item$base
    # global? what if different font?
    glyphs[[base]] <- makeGlyph(base)
    # TODO: random color if not found
    colors[[base]] <- item$color
  }
  maxInfo <- calcMaxInformation(isAminoAcid)
  if (end == 0) {
    end <- length(alignment[[1]])
  }
  totalCols <- end - start + 1
  # new plot
  graphics::plot(0, 0,
       xlim = c(0, totalCols),
       ylim = c(0, maxInfo),
       type = "n")
  # correction?
  correction <- 0
  if (calcCorrection) {
    numSeq <- length(alignment)
    correction <- smallSampleCorrection(numSeq, isAminoAcid)
  }
  for (i in start:end) {
    currCol <- Biostrings::subseq(alignment, i, i)
    currFreqs <- getFrequencies(currCol)
    currInfo <- calcInformation(currFreqs, correction, isAminoAcid)
    heights <- calcHeights(currFreqs, currInfo)
    orderedHeights <- heights[order(heights)]
    currNames <- names(orderedHeights)
    plotColStart <- i - start
    currBottom <- 0
    # draw TODO: refactor this out
    for (j in 1:length(orderedHeights)) {
      base <- currNames[j]
      height <- orderedHeights[[j]]
      glyphPoly(glyphs[[base]],
                bbox = c(plotColStart, currBottom, plotColStart + 1, currBottom + height),
                fill = colors[[base]])
      currBottom <- currBottom + height
    }
  }
}

# aa_set <- names(AMINO_ACID_CODE)
#
# theSettings <- list()
# randomColors <- colorRampPalette(c("#C27E7E", "#816EBA", "#758AC9", "#82C9B6"))(length(aa_set))
#
#
# for (i in 1:length(aa_set)) {
#   someList <- list()
#   someList[["base"]] <- aa_set[i]
#   someList[["color"]] <-randomColors[i]
#   theSettings[[aa_set[i]]] <- someList
# }

# sequenceLogoR(z, theSettings, isAminoAcids = TRUE, start = 200, end = 220)
