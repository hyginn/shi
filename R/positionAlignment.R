library(devtools)

#' positionAlignment.R
#'
#' \code{<function>} Return a matrix containing the position index of the letters int he alignment.
#' The index of a '-' are valued as -1 and do not increment the index of the next letter in the alignment.
#' The matrix is utilized in order to produce the TC score of the reference alignment with the test
#' alignment.
#'
#' Details.
#' @section Input: A sequence alignment in the form of a XStringSet, the number of sequences in the
#' alignment and the width of the sequences (the number of letters in the seuqences of the alignment).
#'
#' @param alignment A sequence alignment produced by readSequence in XStringSet format.
#'
#' @param numSeq The number of sequences in the alignment in the form of an integer.
#'
#' @param maxWidth The total width of the sequences in the alignment. Assume that the alignment produces
#' sequences of the same width.
#'
#'@return A matrix of the positions indicating the position of each letter. Gaps do not count as
#'a position in the sequence and will be denoted by -1.
#'
positionAlignment <- function(alignment, numSeq, maxWidth) {

  #Setting up vectors that temporarily store the positions of each
  #letter in one of the sequences in the alignment
  position <- c()
  temp <- c()

  #Retrieve one of the sequence being compared from an alignment
  for (i in 1:numSeq) {
    count <- 1
    seq <- unlist(strsplit(toString(alignment[i]), ""))

    #Produce the position vector of that sequence
    #Assign -1 to gaps and do not count gaps as a position in the sequence
    for (j in 1:maxWidth) {
      if (identical(seq[j], "-")) {
        temp[j] <- -1
      }

      else {
        temp[j] <- count
        count <- count + 1

      }
    }
    position <- c(position, temp)
  }

  #Produce a matrix containing the positions of the indices
  positionMatrix <- matrix(position, nrow = maxWidth, ncol = numSeq)

  return (positionMatrix)
}
