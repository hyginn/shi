# getFrequencies.R

#' \code{getFrequencies} get frequencies of the residues for a column.
#'
#' Generate a list that contains the frequencies of the residues given a vector
#' of residues. Pseudo counts can be added to each residue to prevent negative
#' frequencies.
#'
#' @param column a vector of residues to calculate the frequencies for.
#' @inheritParams sequenceLogoR
#' @return A table of frequencies for the residues.
#' @seealso \url{https://en.wikipedia.org/wiki/Jeffreys_prior}
#' @export
getFrequencies <- function(column,
                           isAminoAcid = FALSE,
                           gapCharacter = "-",
                           pseudoCountsValue = 0) {
  if (isAminoAcid) {
    alphabet <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I",
                  "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  } else {
    alphabet <- c("A", "T", "C", "G")
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
  freqs <- freqs + pseudoCountsValue
  return(freqs / sum(freqs))
}
