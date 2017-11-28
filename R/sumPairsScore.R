# sumPairsScore.R


#' \code{sumPairsScore} Generate a sum of pairs score (SPS) between two sequences using a reference
#' distance map and a testing distance map for both sequences.
#'
#' The balibase sum of pairs score is also known as the prefab Q score and developer score.
#' It measures what proportion of all residue pairs within columns of one alignment
#' are retained in a comparison alignment.
#'
#' @param refSeqMap A numeric vector for the distance map of reference sequence
#' @param testSeqMap A numeric vector for the distance map of test sequence
#' @param seqLength The number of alignments to compare of the two sequences
#'
#' @return Return the SPS score between the two sequences. That is the number of correctly
#' aligned residue pairs from the test alignment by the number of aligned residue pairs in
#' the reference alignment.
#'
#' @examples
#' refSeq <- c("Q", "-", "L", "R", "-", "K")
#' testSeq <- c ("-", "Q", "L", "R", "S", "K")
#' refSeq2 <- c("Q", "-", "L", "R", "S", "K")
#' testSeq2 <- c("Q", "-", "L", "R", "-", "K")
#' map <- generatePairMap(refSeq, testSeq)
#' map2 <- generatePairMap(refSeq2, testSeq2)
#' sumPairsScore(map$pairMapA, map$pairMapB)
#' sumPairsScore(map2$pairMapA, map2$pairMapB)
#'
#' @references
#' Edgar, Robert C. “Quality Measures for Protein Alignment Benchmarks.”
#' Nucleic Acids Research 38.7 (2010): 2145–2153. PMC. Web. 28 Nov. 2017.
#'
#' The generatePairMap function created by Adriel is used to generate the sequence
#' maps for the parameters.
#'
#' @export

sumPairsScore <- function(refSeqMap, testSeqMap, seqLength) {

  pairCount <- 0
  correctPairCount <- 0
  for (pos in 1: seqLength) {
    refpos <- refSeqMap[pos]

    if (is.na(refpos)) {
      next
    }

    pairCount <- pairCount + 1

    testpos <- testSeqMap[pos]

    if (is.na(testpos)) {
      next
    }

    if (refpos == testpos) {
      correctPairCount <- correctPairCount + 1
    }

    if (0 == pairCount) {
      return(0)
    }
  }

  return (correctPairCount / pairCount)
}


# [END]
