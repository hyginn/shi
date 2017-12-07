#Error in read.fasta(h) : no line starting with a > character found
#In addition: Warning message:
#  In file(con, "r") :
#  file("") only supports open = "w+" and open = "w+b": using the former
devtools::load_all()

# Grabbing the reference alignments in fasta format
referencePath <- system.file("extdata", "refAPSES.mfa", package = "shi")
referenceAlignment <- read.fasta(referencePath)

# The smallest sequence length to compare all sequences up to that position
sequenceLength <- min(c(getLength(referenceAlignment)))

# Generating the reference pair map using the function created by Adriel
referencePairMap <- generatePairWiseMap(referencePath)

# Grabbing the observed test alignments
alignedFolder <- system.file("extdata", "aligned", package = "shi")
alignedFiles <- dir(alignedFolder)

for (i in 1:length(alignedFiles)) {
  targetFile <- alignedFiles[i]
  alignedFilePath <- sprintf("%s/%s", alignedFolder, targetFile)
  # Generating the test pair map using the function created by Adriel
  testPairMap <- generatePairWiseMap(alignedFilePath)

  totalSumPairsScore <- 0
  print(sprintf("Sum pairs score for: %s", targetFile))

  # We will go through every sequence and comparing each sequence with every sequences
  for (j in 1:(length(allKeys) - 1)) {
    seqA <- allKeys[j]
    for (k in (j + 1):length(allKeys)) {
      seqB <- allKeys[k]

      pairSumPairsScore <- sumPairsScore(referencePairMap[[seqA]][[seqB]],
                                         testPairMap[[seqA]][[seqB]], sequenceLength)
      print(sprintf("Pair wise score for %s and %s: %f",
                    seqA, seqB, pairSumPairsScore))

      totalSumPairsScore <- totalSumPairsScore + pairSumPairsScore
    }
  }
  print(sprintf("Total sum pairs score for %s: %f",
                targetFile, totalSumPairsScore))
  print("==========================================")
}