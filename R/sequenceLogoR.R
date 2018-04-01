# sequenceLogoR.R
# TODO: add doc for refDist and pseudocounts
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
#' @param displayGapInfo whether to display the gap informations.
#' @export
sequenceLogoR <- function(alignment,
                          settingsMap,
                          isAminoAcid,
                          start = 1,
                          end = 0,
                          gapCharacter = '-',
                          calcCorrection = FALSE,
                          entropyMethod = "kl",
                          displayGapInfo = FALSE,
                          refDistribution,
                          addPseudoCounts) {
  ##### Param checking
  if (missing(isAminoAcid)) {
    stop("isAminoAcid is not supplied!")
  }
  if (displayGapInfo && !calcCorrection) {
    stop("Can not show gap info without calculating correction!")
  }


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

  ###### Generate equiprobable dist if not supplied and simulated

  ###### Generate sequence logo for the whole alignment end is not specified
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

  ###### Generate correction closure function which contains an internal cache
  if (calcCorrection) {
    smallSampleCorrectionFunc <- smallSampleCorrectionClosure(numSeqs,
                                                              isAminoAcid,
                                                              entropyMethod,
                                                              refDistribution,
                                                              addPseudoCounts)
  }

  ###### iterate though all columns
  for (i in start:end) {
    currCol <- Biostrings::subseq(alignment, i, i)

    ####### Calculate the information for the paricular column
    currFreqs <- getFrequencies(currCol, isAminoAcid, addPseudoCounts)
    # uncorrected information
    currInfo <- calcInformation(currFreqs, entropyMethod, isAminoAcid)

    ###### Make correction if specified
    if (calcCorrection) {
      numObserved <- length(currCol[currCol != gapCharacter])
      correctedForObserved <- smallSampleCorrectionFunc(numObserved, currInfo)
      infoToCalcHeight <- max(correctedForObserved, 0)
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
        graphics::rect(plotColStart, -0.05, plotColStart + 0.95,
                       -gapInformation, col="grey87")
      }
    }
  }
}
