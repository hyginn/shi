library(devtools)

#' balibaseTcScore.R
#'
#' \code{<function>} Return a score for a given multiple sequence alignment compared with a
#' reference alignment. The balibase TC (total column score) is the number of correctly aligned
#' columns divided by the number of columns in the reference alignment.
#'
#' Details.
#' @section Input: A test alignment and reference alignment are provided in fasta format and are
#' reproduced as XStringSet objects by the helper function readSequence.
#'
#' @param testAli The name of the file that contains the MSA of multiple sequences in fasta format.
#' This file must be located in the extdata directory.
#'
#' @param refAli The name of the file that contains the reference MSA of testAli in fasta format.
#' This file must be located in the extdata directory.
#'
#' @param readType Specifies which XStringSet object to produce.
#' This can be either "AAStringSet", "DNAStringSet", or "RNAStringSet". If the provided XStringSet
#' format is invalid, stop execution and return an error statement.
#'
#'@return The balibase TC (total column score); the number of correctly aligned
#' columns divided by the number of columns in the reference alignment.

balibaseTcScore <- function(testAli, refAli, readType) {

  #Count variable to keep track of the number of correct alignments
  #in testSeq compared to refSeq
  correctCount <- 0

  #Call helper function readSequence
  testSeq <- readSequence(testAli, readType)
  refSeq <- readSequence(refAli, readType)

  #Retrieve the length of the longest sequence in the test sequence
  maxWidth <- (max(width(testSeq)))

  #Retrieve the matrix of positions for each alignment
  numSeq <- length(subseq(testSeq, 1, 1))
  testPosition <- positionAlignment(testSeq, numSeq, maxWidth)
  refPosition <- positionAlignment(refSeq, numSeq, maxWidth)

  for (i in 1:maxWidth) {
    #Retrieve a column at a given index of each alignment
    testCol <- c(subseq(testSeq, i, i))
    refCol <- c(subseq(refSeq, i, i))

    #Call helper function compareColumn
    if (compareColumn(testCol, refCol, testPosition[i,], refPosition[i,]) == TRUE) {
      correctCount <- correctCount + 1
    }
  }

  score <- (correctCount / (max(width(refSeq))))
  return (score)
}

