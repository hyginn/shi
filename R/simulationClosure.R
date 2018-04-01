simulationClosure <- function(numTrials,
                              isAminoAcid,
                              refDistribution,
                              entropyMethod,
                              addPseudoCounts = TRUE) {
  if (isAminoAcid) {
    alphabet <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I",
                  "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  } else {
    alphabet <- c("A", "T", "C", "G")
  }
  if (entropyMethod == "kl" && !addPseudoCounts) {
    warning("Forcing addPseudoCounts to TRUE to prevent zero frequencies!")
    addPseudoCounts <- TRUE
  }
  closure <- function(numSamples) {
    IObs <- numeric(numTrials)
    for (i in 1:numTrials) {
      obs <- sample(alphabet,
                    size=numSamples,
                    prob=refDistribution,
                    replace=TRUE)
      # prob need to pass gapchar here
      freqs <- getFrequencies(obs,
                              isAminoAcid,
                              addPseudoCounts=addPseudoCounts)
      IObs[i] <- calcInformation(freqs,
                                 entropyMethod,
                                 isAminoAcid,
                                 refDistribution)
    }
    return(IObs)
  }
  return(closure)
}
