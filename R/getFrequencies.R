# getFrequencies.R

#' \code{getFrequencies} get frequencies of the residues for a column.
#'
#' Details.
#' @param column a column from an alignment
#' @param gapCharacter symbol depicting a gap in the alignment.
#' @param includeGaps whether to consider gaps part of the frequency
#' calculation.
#' @return A table of frequencies for the residues.
#' @export
getFrequencies <- function(column, gapCharacter = '-', includeGaps = FALSE) {
  if (!includeGaps) {
    column <- column[column != gapCharacter]
  }
  return(BiocGenerics::table(column) / length(column))
}