calcShannonEntropy <- function(freqs) {
  total <- 0
  for (i in seq_along(freqs)) {
    currFreq <- as.numeric(freqs[i])
    if (currFreq > 0) {
      total <- total - (currFreq * log2(currFreq))
    }
  }
  return(total)
}

calcKLdiv <- function(p, q) {
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
