# calcShannonEntropy.R

#' \code{calcShannonEntropy} calculate the shannon entropy from a residue
#' frequency of a column.
#'
#' Details.
#' @param freqs frequency table of the residues.
#' @return the shannon entropy of the column.
#' @export
calcShannonEntropy <- function(freqs) {
  return(-sum(freqs * log2(freqs)))
}