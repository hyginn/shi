# simulationClosure.R

#' \code{simulationClosure} generate a closure that will have the same
#' parameters for future simulation calls.
#'
#' To have a simple and conistent way of generating simulations of the same
#' parameters (same number of trials, sample from amino acids
#' or nucleotides, use probabilities from a reference distribution, entropy
#' method, and the amount to use for pseudo counts), a closure is generated. The
#' only variable that differ between future calls is the size of the samples per
#' trial.
#'
#' @param numTrials the number of times a sample should be made.
#' @inheritParams sequenceLogoR
#' @return A closure function to generate simulations of size closure input
#' parameter, numSamples, numTrials times.
simulationClosure <- function(numTrials,
                              isAminoAcid = FALSE,
                              gapCharacter = "-",
                              refDistribution,
                              entropyMethod,
                              pseudoCountsValue = 0) {
  if (isAminoAcid) {
    alphabet <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I",
                  "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  } else {
    alphabet <- c("A", "T", "C", "G")
  }
  # order the alphabet
  alphabet <- sort(alphabet)
  if (entropyMethod == "kl" && pseudoCountsValue == 0) {
    msg <- "Setting pseudoCountsValue to non zero to prevent zero frequencies!"
    warning(msg)
    pseudoCountsValue <- 0.001
  }
  closure <- function(numSamples) {
    IObs <- numeric(numTrials)
    for (i in 1:numTrials) {
      obs <- sample(alphabet,
                    size=numSamples,
                    prob=refDistribution,
                    replace=TRUE)
      freqs <- getFrequencies(obs,
                              isAminoAcid,
                              gapCharacter,
                              pseudoCountsValue)
      IObs[i] <- calcInformation(freqs,
                                 isAminoAcid,
                                 entropyMethod,
                                 refDistribution)
    }
    return(IObs)
  }
  return(closure)
}
