# sequenceLogoR.R

#' \code{sequenceLogoR} generate a sequence logo from a multiple sequence
#' alignment
#'
#' Details.
#' @param alignment a multiple sequence alignment
#' @param settingsMap something something color to base.
#' @param isAminoAcid flag to use amino acid specific calculations.
#' @param start generate the sequence logo starting from this index.
#' @param end generate the sequence logo until this index.
#' @param gapCharacter the chracter dipicting a gap in the alignment.
#' @param calcCorrection flag to calculate small sample corrections.
#' @param entropyMethod choice between shannon or kl (kullback leibler) on
#' calculating the entropy.
#' @export
sequenceLogoR <- function(alignment,
                          settingsMap,
                          isAminoAcid = FALSE,
                          start = 1,
                          end = 0,
                          gapCharacter = '-',
                          calcCorrection = FALSE,
                          entropyMethod = "shannon") {
  # generate glyphs
  glyphs <- list()
  colors <- list()
  for (item in settingsMap) {
    base <- item$base
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
  if (entropyMethod == "shannon" && calcCorrection) {
    numSeqs <- length(alignment)
    corrections <- numeric(numSeqs)
    for (i in 1:numSeqs) {
      corrections[i] <- smallSampleCorrection(i, isAminoAcid, simulated = FALSE)
    }
  }
  for (i in start:end) {
    currCol <- Biostrings::subseq(alignment, i, i)
    correction <- 0
    if (calcCorrection) {
      numNonGaps <- length(currCol[currCol != gapCharacter])
      correction <- corrections[numNonGaps]
    }
    currFreqs <- getFrequencies(currCol)
    currInfo <- calcInformation(currFreqs, entropyMethod,
                                correction, isAminoAcid)
    # remove negative info
    currInfo[currInfo < 0] <- 0
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
                bbox = c(plotColStart, currBottom,
                         plotColStart + 1, currBottom + height),
                fill = colors[[base]])
      currBottom <- currBottom + height
    }
  }
}
