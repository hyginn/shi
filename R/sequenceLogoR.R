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
#' @param simulate flag to use simulations for small sample corrections.
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
                          pseudoCountsValue = 0.001) {
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

  ###### iterate through all the columns and calculate the information
  uncorrectedInformation <- numeric(end - start + 1)
  maxInfoToPlot <- calcMaxInformation(isAminoAcid)
  frequencies <- list()

  if (calcCorrection) {
    correctedInformationForObserved <- numeric(end - start + 1)
    if (displayGapInfo) {
      correctedInformationForAll <- numeric(end - start + 1)
    }
  }
  for (colPos in start:end) {
    currCol <- Biostrings::subseq(alignment, colPos, colPos)
    vectorPos <- colPos - start + 1

    ####### Calculate the frequencies
    currFreqs <- getFrequencies(currCol, isAminoAcid,
                                gapCharacter, pseudoCountsValue)
    frequencies[[colPos]] <- currFreqs

    ####### calculate the uncorrected information
    currInfo <- calcInformation(currFreqs, isAminoAcid,
                                entropyMethod, refDistribution)
    uncorrectedInformation[vectorPos] <- currInfo
    infoToCheckPlotHeight <- currInfo

    ####### calculate the corrections if specified
    if (calcCorrection) {
      numObserved <- length(currCol[currCol != gapCharacter])
      correctedForObserved <- smallSampleCorrectionFunc(numObserved, currInfo)
      correctedInformationForObserved[vectorPos] <- correctedForObserved
      infoToCheckPlotHeight <- correctedForObserved

      ####### Calculate the correction information if displayGapInfo is enabled
      if (displayGapInfo) {
        correctedForAll <- smallSampleCorrectionFunc(numSeqs, currInfo)
        correctedInformationForAll[vectorPos] <- correctedForAll
        infoToCheckPlotHeight <- correctedForAll
      }
    }

    # A column information can be greated than the max information from
    # calcInformation for KL sequence logos
    if (infoToCheckPlotHeight > maxInfoToPlot) {
      maxInfoToPlot <- infoToCheckPlotHeight
    }
  }


  ###### Draw the sequence logo
  if (displayGapInfo) {
    yMin <- -maxInfoToPlot
  } else {
    yMin <- 0
  }

  # new plot
  graphics::plot(0, 0,
                 xlim = c(start, end + 1),
                 ylim = c(yMin, maxInfoToPlot),
                 type = "n",
                 xlab = NULL,
                 ylab = NULL)

  ###### iterate though all columns
  for (colPos in start:end) {
    vectorPos <- colPos - start + 1

    if (calcCorrection) {
      infoToCalcHeight <- correctedInformationForObserved[vectorPos]
      infoToCalcHeight <- max(0, infoToCalcHeight)
    } else {
      infoToCalcHeight <- uncorrectedInformation[vectorPos]
    }

    ###### Calculate heights
    currFreqs <- frequencies[[colPos]]
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
                bbox = c(colPos, currBottom,
                         colPos + 0.95, currBottom + height),
                fill = colors[[base]])
      currBottom <- currBottom + height
    }

    ###### Draw the gap information
    if (displayGapInfo && calcCorrection) {
      correctedForAll <- correctedInformationForAll[vectorPos]
      correctedForAll <- max(0, correctedForAll)
      if (correctedForAll != infoToCalcHeight) {
        gapInformation <- correctedForAll - infoToCalcHeight
        graphics::rect(colPos, -0.05, colPos + 0.95,
                       -gapInformation, col="grey87")
      }
    }
  }
}
