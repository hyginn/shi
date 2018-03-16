# calcMaxInformation.R

#' \code{calcMaxInformation} calculate the maximum information for a column in a
#' sequence logo.
#'
#' Details.
#' @param isAminoAcid flag to use amino acid specific calculations.
#' @return max bits for a single column in a sequence logo.
#' @export
calcMaxInformation <- function(isAminoAcid=FALSE) {
  b <- 4
  if (isAminoAcid) {
    b <- 20
  }
  return(log2(b))
}
