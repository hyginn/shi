# calcInformation.R

#' \code{calcInformation} calculate the information for a column.
#'
#' Details.
#' @param freqs frequency table of the residues.
#' @param entropyMethod method to use to calculate the entropy.
#' @param isAminoAcid flag to use amino acid specific calculations.
#' @param refFreqs the reference frequency table to use with kl divergence.
#' @return A table of frequencies for the residues.
#' @export
calcInformation <- function(freqs, entropyMethod = "kl", isAminoAcid = FALSE,
                            refFreqs = NA) {
  allowedMethods <- c("shannon", "kl")
  if (! entropyMethod %in% allowedMethods) {
    stop('Unknown entropy method! Use "shannon" or "kl"')
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
    entropy <- calcKLdiv(refFreqs, freqs)
    return(entropy)
  }
}