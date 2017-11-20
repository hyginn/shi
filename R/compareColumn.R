library(devtools)

#' compareColumn.R
#'
#' \code{<function>} Return TRUE or FALSE if two given columns from an MSA are equivalent.
#' compareColumn is called in balibaseTcScore as a helper function.
#'
#' Details.
#' @section Input: If a multiple sequence alignment and a reference alignment are provided
#' which are not in multi-Fasta formats, they are converted to multi-Fasta format using
#' the provided shiWriteALN function.
#'
#' @param testCol The name of the file that contains the MSA of multiple sequences in fasta format.
#' This file must be located in the extdata directory.
#'
#' @param refCol The name of the file that contains the reference MSA of testAli in fasta format.
#' This file must be located in the extdata directory.
#'
#'

compareColumn <- function(testCol, refCol) {

  #boolVector carries TRUE at each position of the column if the testCol and the refCol
  #have the same protein/DNA/RNA letter at the given position
  boolVector <- (testCol == refCol)

  #If one of the letters from the testCol do not match with the refCol, return False.
  #Otherwise return True
  for (i in 1:length(boolVector)) {
    if (identical(boolVector[i], FALSE)) {
      return (FALSE)
    }
  }

  return (TRUE)
}