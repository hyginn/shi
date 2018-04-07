# calcEntropy.R

#' \code{calcShannonEntropy} calculate the shannon entropy from a frequency
#' table.
#'
#' @inheritParams calcInformation
#' @seealso \url{https://en.wikipedia.org/wiki/Entropy_(information_theory)}
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

#' \code{calcKLdiv} calculate the Kullback-Leibler divergence.
#'
#' \code{p} and \code{q} are two pmfs of discrete probability distributions
#' with the same outcomes, which are nowhere 0.
#'
#' @param p first pmf
#' @param q second pmf
#' @seealso \url{https://en.wikipedia.org/wiki/Kullback–Leibler_divergence}
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
