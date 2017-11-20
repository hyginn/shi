library(devtools)

#' balibaseTcScore.R
#'
#' \code{<function>} Return a score for a given multiple sequence alignment compared with a
#' reference alignment. The balibase TC (total column score) is the number of correctly aligned
#' columns divided by the number of columns in the reference alignment.
#'
#' Details.
#' @section Input: If a multiple sequence alignment and a reference alignment are provided
#' which are not in multi-Fasta formats, they are converted to multi-Fasta format using
#' the provided shiWriteALN function.
#'
#' @param testAli The name of the file that contains the MSA of multiple sequences in fasta format.
#' This file must be located in the extdata directory.
#'
#' @param refAli The name of the file that contains the reference MSA of testAli in fasta format.
#' This file must be located in the extdata directory.
#'
#'

balibaseTcScore <- function(testAli, refAli, readType) {
  correctCount <- 0
  testSeq <- readSequence(testAli, readType)
  refAli <- readSequence(refAli, readType)
  print(testSeq)

  maxWidth <- ((max(width(testSeq)))[1])

  for (i in 1:maxWidth) {

    testCol <- c(subseq(testSeq, i, i))
    refCol <- c(subseq(refAli, i, i))

    if (compareColumn(testCol, refCol) == TRUE) {
      correctCount <- correctCount + 1
    }
  }

  score <- (correctCount / (max(width(refAli))))
  return (score)
}

