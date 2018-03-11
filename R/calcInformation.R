# calcInformation.R

#' \code{calcInformation} calculate the information for a column.
#'
#' Details.
#' @param freqs frequency table of the residues.
#' @param entropyMethod method to use to calculate the entropy.
#' @param isAminoAcid flag to use amino acid specific calculations.
#' @return A table of frequencies for the residues.
#' @export
calcInformation <- function(freqs, entropyMethod = "kl", isAminoAcid = FALSE) {
  allowedMethods <- c("shannon", "kl")
  if (! entropyMethod %in% allowedMethods) {
    # something warning terminate
  }
  b <- 4
  if (entropyMethod == "shannon") {
    entropy <- calcShannonEntropy(freqs)
    return(calcMaxInformation(isAminoAcid) - entropy)
  } else if (entropyMethod == "kl") {
    # something
  }
}