# smallSampleCorrectionClosure.R

#' \code{smallSampleCorrectionClosure} generate a closure that will have the
#' same parameters for future small smaple correction calculations.
#'
#' To have a simple and consistent way of calculating the small sample
#' corrections of the same parameters (whether it's amino acids, to simulate,
#' entropy method, the reference distribution to calculate against, and the
#' amount to use for pseudo counts) as well to cache prior calculations, a
#' closure is generated. The only variable that differ between future calls is
#' the number of observed samples and the information to correct. Caching is
#' required because subsequent simulations for the same sample size is
#' reduendant.
#'
#' @param numSeqs the number of sequence for the alignment.
#' @param displayDistributions have the ability to inspect the distributions of
#' the simulations.
#' @inheritParams sequenceLogoR
#' @seealso \url{https://en.wikipedia.org/wiki/Sequence_logo}
#' @export
smallSampleCorrectionClosure <- function(numSeqs,
                                         isAminoAcid = FALSE,
                                         gapCharacter = "-",
                                         simulate = FALSE,
                                         entropyMethod = "kl",
                                         refDistribution,
                                         pseudoCountsValue = 0,
                                         displayDistributions = FALSE) {
  # setup cache
  if (simulate) {
    cache <- list()
  } else {
    cache <- numeric(numSeqs)
  }
  calculated <- logical(length = numSeqs)

  # generate the closure
  if (simulate) {
    # A simulation closure is generated to have consistent simulations
    simFunc <- simulationClosure(numTrials = 10000,
                                 isAminoAcid,
                                 gapCharacter,
                                 refDistribution,
                                 entropyMethod,
                                 pseudoCountsValue)
    closure <- function(numObserveredSamples, info) {
      if (calculated[numObserveredSamples]) {
        currSim <- cache[[numObserveredSamples]]
        correction <- sum(currSim <= info) / length(currSim)
      } else {
        # do simulation for the num observed samples
        currSim <- simFunc(numObserveredSamples)
        cache[[numObserveredSamples]] <<- currSim
        correction <- sum(currSim <= info) / length(currSim)
        calculated[numObserveredSamples] <<- TRUE
      }
      if (displayDistributions) {
        hist(currSim, col = "#C9F4E3", breaks = 25)
        abline(v = info, col = "#AA00CC")
      }
      return(correction * info)
    }
  } else {
    # basic correction
    if (isAminoAcid) {
      numRes <- 20
    } else {
      numRes <- 4
    }
    closure <- function(numObserveredSamples, info) {
      if (calculated[numObserveredSamples]) {
        correction <- cache[numObserveredSamples]
      } else {
        correction <- ((numRes - 1) /
                         (log(2) * 2 * numObserveredSamples))
        cache[numObserveredSamples] <<- correction
        calculated[numObserveredSamples] <<- TRUE
      }
      return(info - correction)
    }
  }
  return(closure)
}
