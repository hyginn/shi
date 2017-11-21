# clineShiftScore.R

#' \code{clineShiftScore} Generate a cline shift score between two sequences
#' using a reference distance map and an observed distance map for both
#' sequences.
#'
#' Generate a cline shift score between two sequences. The clineshift score is
#' a distance score between the expected residue index and the observed residue
#' index.
#'
#' @param refMapA A numeric vector for the distance map of reference sequence A.
#' @param refMapB A numeric vector for the distance map of reference sequence B.
#' @param testMapA A numeric vector for the distance map of test sequence A.
#' @param testMapB A numeric vector for the distance map of test sequence B.
#' @param epsilon A scoring parameter. Defaults to 0.2.
#' @return The clineshift score between two sequences.
#'
#' @examples
#' refA <- c("A", "V", "T", "-", "-", "D")
#' refB <- c("A", "V", "T", "F", "F", "D")
#' testA <- c("A", "V", "-", "-", "T", "D")
#' testB <- c("A", "V", "T", "F", "F", "D")
#' refMap <- generatePairMap(refA, refB)
#' testMap <- generatePairMap(testA, testB)
#' clineShiftScore(refMap$pairMapA, refMap$pairMapB,
#'                 testMap$pairMapA, testMap$pairMapB)
#'
#' @references
#' Cline M, Hughey R, Karplus K: Predicting reliable regions in protein
#' sequence alignments.
#' Bioinformatics 2002, 18: 306–314. 10.1093/bioinformatics/18.2.306
#'
#' @export
clineShiftScore <- function(refMapA,
                       refMapB,
                       testMapA,
                       testMapB,
                       epsilon = 0.2) {

  if (length(refMapA) != length(testMapA)) {
    stop("PANIC: maps for sequence A must be of the same length")
  }

  if (length(refMapB) != length(testMapB)) {
    stop("PANIC: maps for sequence B must be of the same length")
  }

  refPairCount <- 0
  testPairCount <- 0
  total <- 0

  mapALength <- length(refMapA)
  mapBLength <- length(refMapB)

  # Calculate total cline shift score for sequence A
  for (i in 1:mapALength) {
    mappedRefPosB <- refMapA[i]
    if (is.na(mappedRefPosB)) {
      next
    }
    refPairCount <- refPairCount + 1
    mappedTestPosB <- testMapA[i]
    if (is.na(mappedTestPosB)) {
      next
    }
    shiftDistance <- abs(mappedRefPosB - mappedTestPosB)
    currColScoreA <- ((1 + epsilon) / (1 + shiftDistance)) - epsilon
    total <- total + currColScoreA
  }

  # Calculate total cline shift score for sequence B
  for (i in 1:mapBLength) {
    mappedTestPosA <- testMapB[i]
    if (is.na(mappedTestPosA)) {
      next
    }
    testPairCount <- testPairCount + 1
    mappedRefPosA <- refMapB[i]
    if (is.na(mappedRefPosA)) {
      next
    }
    shiftDistance <- abs(mappedTestPosA - mappedRefPosA)
    currColScoreB <- ((1 + epsilon) / (1 + shiftDistance)) - epsilon
    total <- total + currColScoreB
  }
  if (refPairCount == 0) {
    warning("WARNING: no aligned pairs in the reference alignement")
    return(0)
  }
  return(total / (refPairCount + testPairCount))
}
