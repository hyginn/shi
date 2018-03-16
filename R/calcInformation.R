# calcInformation.R

#' \code{calcInformation} calculate the information for a column.
#'
#' Details.
#' @param freqs frequency table of the residues.
#' @param entropyMethod method to use to calculate the entropy.
#' @param isAminoAcid flag to use amino acid specific calculations.
#' @return A table of frequencies for the residues.
#' @export
calcInformation <- function(freqs, entropyMethod = "kl", isAminoAcid = FALSE,
                            refFreqs = NA) {
  allowedMethods <- c("shannon", "kl")
  if (! entropyMethod %in% allowedMethods) {
    # something warning terminate
  }
  b <- 4
  if (isAminoAcid) {
    b <- 20
  }
  maxInfo <- calcMaxInformation(isAminoAcid)
  if (entropyMethod == "shannon") {
    entropy <- calcShannonEntropy(freqs)
    return(maxInfo - entropy)
  } else if (entropyMethod == "kl") {
    # generate equiprobable ref
    if (is.na(refFreqs)) {
      refFreqs <- rep(1, b) / b
    }
    entropy <- KLdiv(refFreqs, freqs)
    return(entropy)
  }
}