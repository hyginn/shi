library(devtools)
library(Biostrings)

#readSequence.R

#' \code{<function>} Produce a sequence alignment by taking a protein sequence
#' given in fasta format
#' Details.
#' @section Input: The name of a fasta file that contains a MSA
#'
#' @param fastaFile Name of the fasta filecontaining an MSA of given sequences in the
#' form of a string
#'
#' @param readType Specifies which XStringSet object to produce.
#' This can be either "AAStringSet", "DNAStringSet", or "RNAStringSet". If the provided XStringSet
#' format is invalid, stop execution and return an error statement.
#'
#' @example
#'

readSequence <- function(fastaFile,
                         readType) {

  #Retrieve the filepath of the multiple sequence alignment file
  filepath <- system.file("extdata", fastaFile, package = "shi")

  #Read the alignment according to the given type of alignment
  if (readType == "AAStringSet") {
    mySeq <- readAAStringSet(filepath)
  }

  else if (readType == "DNAStringSet") {
    mySeq <- readDNAStringSet(filepath)
  }

  else if (readType == "RNAStringSet") {
    mySeq <- readRNAStringSet(filepath)
  }

  else {
    stop(sprintf("Input object of class %s is not supported.", readType))
  }

  return (mySeq)
}
