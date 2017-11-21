library(devtools)

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
#'@return TRUE if the reference column and test column have an equivalent alignment at a given
#'position in the MSA. FALSE otherwise.

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