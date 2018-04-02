# calcHeights.R

#' \code{calcHeights} calculate the heights of each residue.
#'
#' The heights of the residues in the sequence logo is proporitional
#' to their frequency in the column
#'
#' @param information the information for the column.
#' @return A table of heights for the residues
calcHeights <- function(freqs, information) {
  return(freqs * information)
}
