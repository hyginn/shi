# sequenceLogoR.R

#' \code{sequenceLogoR} generate a sequence logo from a multiple sequence
#' alignment
#'
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
#' @param refDistribution a table containing the distribution to sample from
#' and/or to calculate the divergence.
#' @param pseudoCountsValue add a small prior to prevent 0 freqs.
#' @export
sequenceLogoR <- function(alignment,
                          settingsMap,
                          isAminoAcid,
                          start = 1,
                          end = 0,
                          gapCharacter = '-',
                          calcCorrection = FALSE,
                          simulate = FALSE,
                          entropyMethod = "shannon",
                          displayGapInfo = FALSE,
                          refDistribution,
                          pseudoCountsValue = 0) {
  ##### Param checking
  if (missing(isAminoAcid)) {
    stop("isAminoAcid is not supplied!")
  }
  if (displayGapInfo && !calcCorrection) {
    stop("Can not show gap info without calculating correction!")
  }
  if (missing(settingsMap)) {
    if (isAminoAcid) {
      alphabet <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I",
                    "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
    } else {
      alphabet <- c("A", "T", "C", "G")
    }
    settingsMap <- generateSettingsMap(alphabet)
  }

  ###### generate glyphs
  glyphs <- list()
  colors <- list()
  for (residueSetting in settingsMap$residueSettings) {
    residue <- residueSetting$residue
    character <- residueSetting$character
    color <- residueSetting$color
    glyphs[[residue]] <- makeGlyph(character, settingsMap$font)
    colors[[residue]] <- color
  }

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
  graphics::plot(start, 0,
       xlim = c(start, end + 1),
       ylim = c(yMin, maxInfo),
       type = "n",
       xlab = NULL,
       ylab = NULL)

  ###### Generate correction closure function which contains an internal cache
  if (calcCorrection) {
    smallSampleCorrectionFunc <- smallSampleCorrectionClosure(numSeqs,
                                                              isAminoAcid,
                                                              gapCharacter,
                                                              simulate,
                                                              entropyMethod,
                                                              refDistribution,
                                                              pseudoCountsValue)
  }

  ###### iterate though all columns
  for (i in start:end) {
    currCol <- Biostrings::subseq(alignment, i, i)

    ####### Calculate the information for the paricular column
    currFreqs <- getFrequencies(currCol, isAminoAcid,
                                gapCharacter, pseudoCountsValue)

    # uncorrected information
    currInfo <- calcInformation(currFreqs, isAminoAcid,
                                entropyMethod, refDistribution)

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
    currBottom <- 0

    ###### Draw out sequence logo
    for (j in 1:length(orderedHeights)) {
      base <- currNames[j]
      height <- orderedHeights[[j]]
      # bbox width should be settable
      glyphPoly(glyphs[[base]],
                bbox = c(i, currBottom,
                         i + 0.95, currBottom + height),
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
