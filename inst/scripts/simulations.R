###### prep

# build package

# required sourced funcions

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

# the smallSampleCorreciton in question
# modified the simulation portion to show plots

smallSampleCorrectionClosure <- function(numSeqs,
                                         isAminoAcid = FALSE,
                                         simulate = FALSE,
                                         entropyMethod = "kl",
                                         refDistribution,
                                         addPseudoCounts = TRUE) {
  # setup cache
  if (simulate) {
    cache <- list()
  } else {
    cache <- numeric(numSeqs)
  }
  calculated <- logical(length = numSeqs)

  # generate the closure
  if (simulate) {
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
      # modification here
      # get i from outer scope
      title <- sprintf("position %d", i)
      print(title)
      print(info)
      hist(currSim, col = "#C9F4E3", breaks = 25, main=title)
      abline(v = info, col = "#AA00CC")
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

###### demo

# sequence to use
if (! require(msa, quietly=TRUE)) {
  if (! exists("biocLite")) {
    source("https://bioconductor.org/biocLite.R")
  }
  biocLite("msa")
  library(msa)
}

mySequenceFile <- system.file("examples", "exampleAA.fasta", package="msa")
mySequences <- readAAStringSet(mySequenceFile)
mySequences

myFirstAlignment <- msa(mySequences, "Muscle")

m <- as.character(myFirstAlignment)
alignment <- AAStringSet(m)

# distributions

AAref <- numeric()  # Uniprot frequencies October 2017, slightly adjusted to
# sum to 1.0
AAref["A"] <- 0.0904
AAref["C"] <- 0.0123
AAref["D"] <- 0.0545
AAref["E"] <- 0.0617
AAref["F"] <- 0.0394
AAref["G"] <- 0.0724
AAref["H"] <- 0.0221
AAref["I"] <- 0.0573
AAref["K"] <- 0.0504
AAref["L"] <- 0.0986
AAref["M"] <- 0.0240
AAref["N"] <- 0.0392
AAref["P"] <- 0.0486
AAref["Q"] <- 0.0381
AAref["R"] <- 0.0570
AAref["S"] <- 0.0673
AAref["T"] <- 0.0558
AAref["V"] <- 0.0686
AAref["W"] <- 0.0129
AAref["Y"] <- 0.0294

equiProb <- rep(1, 20) / 20
names(equiProb) <- names(AAref)

# func setup
correction1 <- smallSampleCorrectionClosure(length(alignment),
                                            isAminoAcid = TRUE,
                                            simulate = TRUE,
                                            entropyMethod = "kl",
                                            refDistribution = AAref,
                                            addPseudoCounts = TRUE)

correction2 <- smallSampleCorrectionClosure(length(alignment),
                                            isAminoAcid = TRUE,
                                            simulate = TRUE,
                                            entropyMethod = "kl",
                                            refDistribution = equiProb,
                                            addPseudoCounts = TRUE)

getInfo <- function(col, refFreqs) {
  freqs <- getFrequencies(col, isAminoAcid = TRUE, gapCharacter = "-", addPseudoCounts = TRUE)
  info <- calcInformation(freqs, entropyMethod = "kl", isAminoAcid = TRUE, refFreqs)
  return(info)
}

for (i in 11:21) {
  currCol <- subseq(alignment, i, i)
  print(currCol)
  info <- getInfo(currCol, AAref)
  numObs <- sum(currCol != "-")
  correction1(numObs, info)
}

for (i in 200:220) {
  currCol <- subseq(alignment, i, i)
  print(currCol)
  info <- getInfo(currCol, AAref)
  numObs <- sum(currCol != "-")
  correction1(numObs, info)
}

for (i in 11:21) {
  currCol <- subseq(alignment, i, i)
  print(currCol)
  info <- getInfo(currCol, equiProb)
  numObs <- sum(currCol != "-")
  correction2(numObs, info)
}


# sequence logo portion
aa_set <- names(AMINO_ACID_CODE)

theSettings <- list()
randomColors <- colorRampPalette(c("#C27E7E", "#816EBA", "#758AC9", "#82C9B6"))(length(aa_set))


for (i in 1:length(aa_set)) {
  someList <- list()
  someList[["base"]] <- aa_set[i]
  someList[["color"]] <-randomColors[i]
  theSettings[[aa_set[i]]] <- someList
}

# no correction
sequenceLogoR(alignment, theSettings, isAminoAcid = TRUE, start = 1, end = 100, entropyMethod = "shannon", refDistribution = AAref, addPseudoCounts = TRUE, simulate = FALSE)
# simulated correction
sequenceLogoR(alignment, theSettings, isAminoAcid = TRUE, start = 1, end = 100, calcCorrection = TRUE, entropyMethod = "shannon", refDistribution = AAref, addPseudoCounts = TRUE, simulate = TRUE)
# simple correction
sequenceLogoR(alignment, theSettings, isAminoAcid = TRUE, start = 1, end = 100, calcCorrection = TRUE, entropyMethod = "shannon", refDistribution = AAref, addPseudoCounts = TRUE, simulate = FALSE)
