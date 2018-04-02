# calcInformation.R

#' \code{calcInformation} calculate the information for a column.
#'
#' From a given \code{freqs} information can be calculated from calculating
#' the information loss using shannon entropy or the relative entropy using
#' Kullback–Leibler divergence.
#'
#' @param freqs frequency table of the residues.
#' @inheritParams sequenceLogoR
#' @return A table of frequencies for the residues.
#' @seealso \url{https://en.wikipedia.org/wiki/Entropy_(information_theory)}
#' @seealso \url{https://en.wikipedia.org/wiki/Kullback–Leibler_divergence}
#' @seealso \code{\link{calcShannonEntropy}}
#' @seealso \code{\link{calcKLdiv}}
#' @export
calcInformation <- function(freqs, isAminoAcid = FALSE, entropyMethod,
                            refDistribution) {
  allowedMethods <- c("shannon", "kl")
  if (! entropyMethod %in% allowedMethods) {
    stop('Unknown entropy method! Use "shannon" or "kl"')
  }
  if (isAminoAcid) {
    b <- 20
  } else {
    b <- 4
  }
  if (entropyMethod == "shannon") {
    maxInfo <- calcMaxInformation(isAminoAcid)
    entropy <- calcShannonEntropy(freqs)
    return(maxInfo - entropy)
  } else if (entropyMethod == "kl") {
    # generate equiprobable ref
    if (missing(refDistribution)) {
      warning("No refDistribution supplied, using equiprobable distribution!")
      refDistribution <- rep(1, b) / b
    }
    entropy <- calcKLdiv(freqs, refDistribution)
    return(entropy)
  }
}