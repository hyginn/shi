smallSampleCorrectionClosure <- function(numSeqs,
                                         isAminoAcid = FALSE,
                                         simulated = FALSE,
                                         entropyMethod = "kl",
                                         refDistribution,
                                         addPseudoCounts = TRUE) {
  # setup cache
  if (simulated) {
    cache <- list()
  } else {
    cache <- numeric(numSeqs)
  }
  calculated <- logical(length = numSeqs)

  # generate the closure
  if (simulated) {
    simFunc <- simulationClosure(numTrials = 10000,
                                 isAminoAcid,
                                 refDistribution,
                                 entropyMethod,
                                 addPseudoCounts)
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
