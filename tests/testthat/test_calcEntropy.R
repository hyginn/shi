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

# TODO: write KL test
