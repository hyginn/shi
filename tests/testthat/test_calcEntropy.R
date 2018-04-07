# test_calcEntropy.R

context("calculate entropy")

test_that("able to calculate shannon entropy", {
  freqs <- numeric(4)
  names(freqs) <- c("A", "T", "C", "G")
  freqs["A"] <- 0.5
  freqs["T"] <- 0.25
  freqs["C"] <- 0.25
  freqs["G"] <- 0
  entropy <- calcShannonEntropy(freqs)
  # ignore G as the log_2 of 0 is -inf
  expectedEntropy <- -((0.5 * log2(0.5)) +
                      (0.25 * log2(0.25)) +
                      (0.25 * log2(0.25)))
  expect_equal(entropy, expectedEntropy)
})

test_that("able to calculate Kullback Leibler divergence", {
  freqs <- numeric(4)
  names(freqs) <- c("A", "T", "C", "G")
  freqs["A"] <- 0.4
  freqs["T"] <- 0.25
  freqs["C"] <- 0.25
  freqs["G"] <- 0.1
  equiProb <- rep(1, 4) / 4
  names(equiProb) <- names(freqs)
  entropy <- calcKLdiv(equiProb, freqs)
  expectedEntropy <- (0.25 * log(0.25 / 0.4)) +
                     (0.25 * log(0.25 / 0.25)) +
                     (0.25 * log(0.25 / 0.25)) +
                     (0.25 * log(0.25 / 0.1))
  expect_equal(entropy, expectedEntropy)
})