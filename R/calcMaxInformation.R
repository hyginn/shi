# calcMaxInformation.R

#' \code{calcMaxInformation} calculate the maximum information for shannon
#' entropy sequence logos.
#'
#' @inheritParams sequenceLogoR
#' @return max bits for a single column in a sequence logo.
calcMaxInformation <- function(isAminoAcid=FALSE) {
  if (isAminoAcid) {
    return(log2(20))
  } else {
    return(log2(4))
  }
}
