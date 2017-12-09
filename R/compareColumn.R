#' compareColumn.R
#'
#' \code{<function>} Return TRUE or FALSE if two given columns from an MSA are equivalent.
#' compareColumn is called in balibaseTcScore as a helper function.
#'
#' Details.
#' @section Input: A testCol column and refCol column subsetted from a XStringSet object. Both
#' inputs carry a column at a given position in the multiple sequence alignment.
#'
#' @param testCol The input column at a given position in the multiple sequence alignment being
#' compared to a reference alignment.
#'
#' @param refCol The input column at a given position in the multiple sequence alignment in the
#' reference alignment.
#'
#' @param testPosition A vector containing the positions of each letter in a column at a given index
#' for the test alignment
#'
#' @param refPosition A vector containing the positions of each letter in a column at a given index for
#' the reference alignment
#'
#'@return TRUE if the reference column and test column have an equivalent alignment at a given
#'position in the MSA. The positions and the letters in each column of the alignment
#'must be equal. Return FALSE otherwise.

compareColumn <- function(testCol, refCol, testPosition, refPosition) {

  #Carries TRUE  if the testCol and the refCol
  #have the same protein/DNA/RNA letter at the given position
  sameLetter <- (toString(testCol) == toString(refCol))

  #Carries TRUE if the positions of each letter are the same
  samePos <- testPosition == refPosition

  #If one of the letters from the testCol do not match with the refCol, or one of the
  #positions in the reference aligment column and the test alignment column do not match,
  #return False.
  #Otherwise return True
  if (identical(sameLetter, FALSE) || (FALSE %in% samePos)) {
      return (FALSE)
  }

  return (TRUE)
}