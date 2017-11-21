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

referencePath <- system.file("extdata", "refAPSES.mfa", package = "shi")
referenceAlignment <- read.fasta(referencePath)
# stripGaps(referencePath, "oki.fasta")

allKeys <- names(referenceAlignment)

keyCount <- length(allKeys)

totalPairs <- (keyCount * (keyCount - 1)) / 2

referencePairMap <- generatePairWiseMap(referencePath)

alignedFolder <- system.file("extdata", "aligned", package = "shi")
alignedFiles <- dir(alignedFolder)

for (i in 1:length(alignedFiles)) {
  targetFile <- alignedFiles[i]
  alignedFilePath <- sprintf("%s/%s", alignedFolder, targetFile)
  testPairMap <- generatePairWiseMap(alignedFilePath)
  totalClineshiftScore <- 0
  print(sprintf("Clineshift score for: %s", targetFile))
  for (j in 1:(length(allKeys) - 1)) {
    seqA <- allKeys[j]
    for (k in (j + 1):length(allKeys)) {
      seqB <- allKeys[k]
      pairClineshiftScore <- clineShiftScore(referencePairMap[[seqA]][[seqB]],
                                             referencePairMap[[seqB]][[seqA]],
                                             testPairMap[[seqA]][[seqB]],
                                             testPairMap[[seqB]][[seqA]])
      print(sprintf("Pair wise score for %s and %s: %f",
                    seqA, seqB, pairClineshiftScore))
      totalClineshiftScore <- totalClineshiftScore + pairClineshiftScore
    }
  }
  print(sprintf("Total clineshift score for %s: %f",
                targetFile, totalClineshiftScore))
  print(sprintf("Fractional clineshift score for %s: %f",
                targetFile, totalClineshiftScore / totalPairs))
  print("==========================================")
}
