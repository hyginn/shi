# test_clineShiftScore.R

context("clineShiftScore")

scoringFunction <- function(shiftDistance, epsilon = 0.2) {
  return((1 + epsilon) / (1 + shiftDistance) - epsilon)
}

test_that("Generate a clineshift score", {
  refMapA <- c(1, 2, 3, 6, NA, NA)
  refMapB <- c(1, 2, 3, NA, NA, 4)
  testMapA <- c(1, 4, 5, 6, NA, NA)
  testMapB <- c(1, NA, NA, 2, 3, 4)

  seqAScores <- abs(c(1 - 1, 2 - 4, 3 - 5, 6 - 6))
  seqBScores <- abs(c(1 - 1, 4 - 4))
  refPairs <- 4
  testPairs <- 4
  seqAClineShift <- sum(scoringFunction(seqAScores))
  seqBClineShift <- sum(scoringFunction(seqBScores))
  expect_equal(clineShiftScore(refMapA, refMapB, testMapA, testMapB),
               (seqAClineShift + seqBClineShift) / (refPairs + testPairs))
})

test_that("A perfect alignment will have a clineshift score of 1", {
  mapA <- c(1, 2, 3, 6, NA, NA)
  mapB <- c(1, 2, 3, NA, NA, 4)
  expect_equal(clineShiftScore(mapA, mapB, mapA, mapB), 1)
})

test_that("There will be an error if the test and ref lengths are different", {
  refMapA1 <- c(1, 2, 3, 6, NA, NA)
  refMapB1 <- c(1, 2, 3, NA, NA, 4)
  testMapA1 <- c(1, 4, 5, 6, NA)
  testMapB1 <- c(1, NA, NA, 2, 3, 4)
  expect_error(clineShiftScore(refMapA1, refMapB1, testMapA1, testMapB1),
               "PANIC: maps for sequence A must be of the same length")
  refMapA2 <- c(1, 2, 3, 6, NA, NA)
  refMapB2 <- c(1, 2, 3, NA, NA)
  testMapA2 <- c(1, 4, 5, 6, NA, NA)
  testMapB2 <- c(1, NA, NA, 2, 3, 4)
  expect_error(clineShiftScore(refMapA2, refMapB2, testMapA2, testMapB2),
               "PANIC: maps for sequence B must be of the same length")
})

