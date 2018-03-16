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
#' @param gapBoxesPosition where to display the gap boxes.
#' @export
sequenceLogoR <- function(alignment,
                          settingsMap,
                          isAminoAcid = FALSE,
                          start = 1,
                          end = 0,
                          gapCharacter = '-',
                          calcCorrection = FALSE,
                          entropyMethod = "shannon",
                          displayGapInfo = FALSE) {
  # TODO: check if gapBoxesPosition is valid
  ###### generate glyphs
  glyphs <- list()
  colors <- list()
  for (item in settingsMap) {
    base <- item$base
    glyphs[[base]] <- makeGlyph(base)
    # TODO: random color if not found
    colors[[base]] <- item$color
  }
  if (end == 0) {
    end <- length(alignment[[1]])
  }
  totalCols <- end - start + 1
  maxInfo <- calcMaxInformation(isAminoAcid)
  numSeqs <- length(alignment)

  ###### new plot
  yMin <- 0
  if (displayGapInfo) {
    if (!calcCorrection) {
      stop("Can't show gap information without calculating correction")
    }
    yMin <- -maxInfo
  }
  graphics::plot(0, 0,
       xlim = c(0, totalCols),
       ylim = c(yMin, maxInfo),
       type = "n")

  ###### Generate list to keep track of calculated corrections
  # correction?
  if (calcCorrection) {
    corrections <- numeric(numSeqs)
    # calculate correction when all sequences are considered
    corrections[numSeqs] <- smallSampleCorrection(numSeqs, isAminoAcid,
                                                  simulated = FALSE)
  }

  ###### iterate though all columns
  for (i in start:end) {
    currCol <- Biostrings::subseq(alignment, i, i)

    ####### Calculate the information for the paricular column
    currFreqs <- getFrequencies(currCol, isAminoAcid, addPseudoCounts = TRUE)
    # uncorrected information
    currInfo <- calcInformation(currFreqs, entropyMethod, isAminoAcid)

    ###### Make correction if specified
    if (calcCorrection) {
      numNonGaps <- length(currCol[currCol != gapCharacter])
      # no correction has been calculated
      # TODO: simulated correction?
      if (corrections[numNonGaps] == 0) {
        corrections[numNonGaps] <- smallSampleCorrection(numNonGaps,
                                                         isAminoAcid,
                                                         simulated = FALSE)
      }
      correctionForObserved <- corrections[numNonGaps]
      correctionForAll <- corrections[numSeqs]
      # TODO: depending on what kind of correction is used, this needs to be
      # rewritten
      correctedInformationObserved <- max(currInfo - correctionForObserved, 0)
      correctedInformationAll <- max(currInfo - correctionForAll, 0)
      infoToCalcHeight <- correctedInformationObserved
    } else {
      infoToCalcHeight <- currInfo
    }

    ###### Calculate heights
    heights <- calcHeights(currFreqs, infoToCalcHeight)
    orderedHeights <- heights[order(heights)]
    currNames <- names(orderedHeights)
    plotColStart <- i - start
    currBottom <- 0

    ###### Draw out sequence logo
    for (j in 1:length(orderedHeights)) {
      base <- currNames[j]
      height <- orderedHeights[[j]]
      # bbox width should be settable
      glyphPoly(glyphs[[base]],
                bbox = c(plotColStart, currBottom,
                         plotColStart + 0.95, currBottom + height),
                fill = colors[[base]])
      currBottom <- currBottom + height
    }

    ###### Draw the gap information
    if (displayGapInfo && calcCorrection) {
      if (correctedInformationAll != correctedInformationObserved) {
        gapInformation <- correctedInformationAll - correctedInformationObserved
        rect(plotColStart, -0.05, plotColStart + 0.95,
             -gapInformation, col="grey87")
      }
    }
  }
}
