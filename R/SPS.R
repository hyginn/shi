library(devtools)

# SPS.R

# The Prefab Q Score also known as the balibase SPS Score and Developer Score.
#' It measures what proportion of all residue pairs within columns of one alignment 
#' are retained in a comparison alignment.
#' 
#' \code{<function>} 
#'
#' Details.
#' @section Input: 
#'
#' @param ali 
#'   
#' @return Return the SPS score that is the number of correctly aligned residue pairs
#' from the test alignment by the number of aligned residue pairs in the reference alignment.
#'
#'

##================================================================
#We have a test alignment of N sequences with M columns
# and for each ith column with Ai1 to AiN, each pair of residues Aij and Aik,
# we have a score of p_ijk = 1 if the corresponding residues are aligned
# with each other in the reference alignment, and 0 otherwise
#seq1 <- "Q-LR-K"
#seq2 <- "-NLRSK"
#seq3 <- "-NLR-S"

#ref1 <- "QLR-K"
#ref2 <- "NLRSK"
#ref3 <- "NLR-S"

#testAli <- c(seq1, seq2, seq3)
#refAli <- c(ref1, ref2, ref3)

#lenAli <- nchar(testAli)
#msaMatrix <- matrix(nrow = length(testAli), ncol = lenAli)

#for (posi in 1: nchar(testAli)){
#  for (i in 1:length(testAli)) {
#  }
#}

# if same letter then score is 1 otherwise -1
#check <- function(pos1, pos2) {
#  if ((pos1 == "-") && (pos2 == "-")) {
#    return(0)
#  }
#  else if ((pos1 == "-") || (pos2 == "-")) {
#    return (-2)
#  }
#  else if (pos1 == pos2) {
#    return (1)
#  }
#  else {
#    return (-1)
#  }
#}
##=======================================================

#Adriel's pair map generator

isGap <- function(element) {
  return(element == "-")
}

generatePairMap <- function(seqA, seqB) {
  posA <- 0
  posB <- 0
  seqALength <- length(seqA)
  seqBLength <- length(seqB)
  stopifnot(seqALength == seqBLength)
  seqAMap <- vector(mode="numeric", length=seqALength)
  seqBMap <- vector(mode="numeric", length=seqBLength)
  pos1 <- 1
  pos2 <- 1
  for (i in 1:seqALength) {
    cA <- seqA[i]
    cB <- seqB[i]
    cAGap <- isGap(cA)
    cBGap <- isGap(cB)
    if (!cAGap && !cBGap) {
      seqAMap[posA] <- posA
      seqBMap[posB] <- posB
      # Handle the non upper case a differt time
      posA <- posA + 1
      posB <- posB + 1
    } else if (!cAGap && cBGap) {
      seqAMap[posA] <- -1
      posA <- posA + 1
    } else if (cAGap && !cBGap) {
      seqBMap[posB] <- -1
      posB <- posB + 1
    }
  }
  df = data.frame(seqAMap, seqBMap)
  return(df)
}

#========================================================

spsScore <- function(refSeqMap, testSeqMap, seqlength) {
  pairCount <- 0
  correctPairCount <- 0
  for (pos in 1: seqlength) {
    refpos <- refSeqMap[pos]

    if (-1 == refpos) {
      next
    }
  
    pairCount <- pairCount + 1
  
    testpos <- testSeqMap[pos]
  
    if (-1 == testpos) {
      next
    }
  
    if (refpos == testpos) {
      correctPairCount <- correctPairCount + 1
    }
  
    if (0 == pairCount) {
      return(0)
    }
  }

  return (correctPairCount / pairCount)
}

testseq <- c('Q', '-', 'L', 'R', '-', 'K')
refseq <- c ('-', 'Q', 'L', 'R', 'S', 'K')

d <- generatePairMap(testseq, refseq)

spsScore(d$seqAMap, d$seqBMap, 6)

testseq2 <- c('Q', '-', 'L', 'R', 'S', 'K')
refseq2 <- c ('Q', '-', 'L', 'R', '-', 'K')

d <- generatePairMap(testseq2, refseq2)

spsScore(d$seqAMap, d$seqBMap, 6)

testseq2 <- c('Q', '-', 'L', 'R', 'S', 'K')
refseq2 <- c ('Q', '-', 'L', 'R', 'S', 'K')

d <- generatePairMap(testseq2, refseq2)

spsScore(d$seqAMap, d$seqBMap, 6)


# [END]
