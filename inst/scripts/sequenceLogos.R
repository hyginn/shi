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

sequenceLogoR(z, theSettings, isAminoAcid = TRUE, start = 200, end = 220, calcCorrection = TRUE)
