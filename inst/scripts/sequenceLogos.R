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
z <- AAStringSet(m)

aa_set <- names(AMINO_ACID_CODE)

theSettings <- list()
randomColors <- colorRampPalette(c("#C27E7E", "#816EBA", "#758AC9", "#82C9B6"))(length(aa_set))


for (i in 1:length(aa_set)) {
  someList <- list()
  someList[["base"]] <- aa_set[i]
  someList[["color"]] <-randomColors[i]
  theSettings[[aa_set[i]]] <- someList
}

sequenceLogoR(z, theSettings, isAminoAcid = TRUE, start = 1, end = 100, entropyMethod = "shannon", refDistribution = AAref, addPseudoCounts = TRUE, simulate = FALSE)
sequenceLogoR(z, theSettings, isAminoAcid = TRUE, start = 1, end = 100, calcCorrection = TRUE, entropyMethod = "shannon", refDistribution = AAref, addPseudoCounts = TRUE, simulate = TRUE)


KLdiv <- function(p, q) {
  # p and q are two pmfs of discrete probability distributions
  # with the same outcomes, which are nowhere 0.
  # Value:  Kullback-Leibler divergence  sum(p * log( p / q))).

  if (length(p) != length(q)) {
    stop("PANIC: input vector lengths differ!")
  }
  if (any(c((p == 0), (q == 0)))) {
    stop("PANIC: 0's found in input vectors!")
  }

  return(sum(p * log( p / q )))
}