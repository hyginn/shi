# generatePairMap.R

#' \code{generatePairMap} Generate a distance pair map between two sequences.
#'
#' Generate a distance pair map between two sequences. A distance pair map keeps
#' track of which residue of sequence A is aligned with to sequence B and vice
#' versa. If a residue is aligned to a gap, the stored value will be \code{NA}.
#'
#'
#' @section Input: The gaps for the two sequence vectors is a character defined
#'   in \code{isGap}. Both sequences must be of the same length.
#'
#' @param seqA A character vector for sequence A.
#' @param seqB A character vector for sequence B.
#' @return A list containing two pair maps for sequence A and B.
#'
#' @examples
#' seqA <- c("A", "V", "T", "-", "-", "D")
#' seqB <- c("A", "V", "T", "F", "F", "D")
#' generatePairMap(seqA, seqB)
#' @export
generatePairMap <- function(seqA,
                            seqB) {

  seqAResidueCount <- sum(!isGap(seqA))
  seqBResidueCount <- sum(!isGap(seqB))
  totalCols <- length(seqA)
  pairMapA <- rep(NA, seqAResidueCount)
  pairMapB <- rep(NA, seqBResidueCount)
  posA <- 1
  posB <- 1
  for (i in 1:totalCols) {
    residueA <- seqA[i]
    residueB <- seqB[i]
    isAGap <- isGap(residueA)
    isBGap <- isGap(residueB)
    if (!isAGap && !isBGap) {
      pairMapA[posA] <- posB
      pairMapB[posB] <- posA
      posA <- posA + 1
      posB <- posB + 1
    } else if (!isAGap && isBGap) {
      pairMapA[posA] <- NA
      posA <- posA + 1
    } else if (isAGap && !isBGap) {
      pairMapB[posB] <- NA
      posB <- posB + 1
    }
  }
  pairMapList <- list("pairMapA" = pairMapA, "pairMapB" = pairMapB)
  return(pairMapList)
}
