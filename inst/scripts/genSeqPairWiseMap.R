# These are Adriel's function that I will be using to test my sumPairsScore.

library(seqinr)

stripGaps <- function(fastaPath, outPath) {
  outputText <- c()
  referenceSequenceAlignment <- read.fasta(fastaPath, seqtype = "AA")
  for (i in 1:length(referenceSequenceAlignment)) {
    seq <- referenceSequenceAlignment[[i]]
    seqName <- attr(seq, "name")
    seqHeader <- sprintf(">%s", seqName)
    outputText <- c(outputText, seqHeader)
    noGaps <- seq[seq != "-"]
    textSeq <- trimws(paste(noGaps, collapse = ""))
    outputText <- c(outputText, textSeq)
  }
  fileConn <- file(outPath)
  writeLines(outputText, fileConn)
  close(fileConn)
}

cleanupSequence <- function(seq) {
  justAString <- paste(seq, collapse="")
  trimmed <- trimws(justAString)
  split <- strsplit(trimmed, "")
  return(split[[1]])
}

generatePairWiseMap <- function(fastaPath) {
  alignment <- read.fasta(fastaPath)
  allKeys <- names(alignment)

  # Generate output list of lists
  pairwisePairMap <- vector(mode="list", length=length(allKeys))
  names(pairwisePairMap) <- allKeys

  for (i in 1:length(allKeys)) {
    seqAName <- allKeys[i]
    subKeys <- allKeys[1:length(allKeys) != i]
    pairwisePairMap[[seqAName]] <- vector(mode="list", length=length(subKeys))
    names(pairwisePairMap[[seqAName]]) <- subKeys
  }
  # Generate all possible pairmaps
  for (i in 1:(length(alignment) - 1)) {
    seqA <- alignment[[i]]
    seqAName <- attr(seqA, "name")
    cleanSeqA <- cleanupSequence(seqA)
    for (j in (i + 1):length(alignment)) {
      seqB <- alignment[[j]]
      seqBName <- attr(seqB, "name")
      cleanSeqB <- cleanupSequence(seqB)
      pairMap <- generatePairMap(cleanSeqA, cleanSeqB)
      pairwisePairMap[[seqAName]][[seqBName]] <- pairMap$pairMapA
      pairwisePairMap[[seqBName]][[seqAName]] <- pairMap$pairMapB
    }
  }
  return(pairwisePairMap)
}