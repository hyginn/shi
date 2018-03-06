# smallSampleCorrection.R

#' \code{smallSampleCorrection} calculate the small sample correction.
#'
#' Details.
#' @param numSamples number of samples being considered.
#' @param isAminoAcid flag to use amino acid specific calculations.
#' @param simulated later
#' @param refDistribution later
#' @return the correction to the sequence logo heights
#' @export
smallSampleCorrection <- function(numSamples, isAminoAcid = FALSE,
                                  simulated = FALSE, refDistribution = FALSE) {
  if (simulated) {
    # do this later
  } else {
    b <- 4
    if (isAminoAcid) {
      b <- 20
    }
    return(b / (log(2) * 2 * numSamples))
  }
}