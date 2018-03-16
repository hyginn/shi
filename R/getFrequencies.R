# getFrequencies.R

#' \code{getFrequencies} get frequencies of the residues for a column.
#'
#' Details.
#' @param column a column from an alignment
#' @param isAminoAcid lag to use amino acid specific calculations.
#' @param gapCharacter symbol depicting a gap in the alignment.
#' @param addPseudoCounts add a small prior to prevent 0 freqs.
#' @return A table of frequencies for the residues.
#' @export
getFrequencies <- function(column, isAminoAcid = FALSE, gapCharacter = "-",
                           addPseudoCounts = FALSE) {
  if (isAminoAcid) {
    alphabet <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I",
                  "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  } else {
    alphabet <- c("A", "C", "T", "G")
  }
  freqs <- numeric(length(alphabet))
  names(freqs) <- alphabet
  for (i in seq_along(column)) {
    residue <- as.character(column[i])
    isGap <- residue == gapCharacter
    if (!(residue %in% alphabet) && !isGap) {
      stop("PANIC: unknown residue!")
    }
    if (!isGap) {
      freqs[residue] <- freqs[residue] + 1
    }
  }
  if (addPseudoCounts) {
    freqs <- freqs + 0.001
  }
  return(freqs / sum(freqs))
}
