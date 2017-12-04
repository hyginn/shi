# isGap.R

#' \code{isGap} checks whether the input is a gap.
#'
#' Checked whether the input is the "-" character.
#'
#' @param toCheck A character vector to check gaps for.
#' @return A vector containing a boolean for each element in the input.
isGap <- function(toCheck) {
  return(toCheck == "-")
}
