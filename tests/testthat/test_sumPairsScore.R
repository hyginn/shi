# test_sumPairsScore.R

context("sumPairsScore")


test_that("Generate a sum of pairs score", {
  refSeqMap <- c(NA, 2, 3, 5, NA, NA)
  testSeqMap <- c(NA, 2, 3, NA, 4, NA)
  correctPairCount <- 4
  pairCount <- 6
  expect_equal((sumPairsScore(refSeqMap, testSeqMap, 6)), correctPairCount/pairCount)
})

test_that("A perfect alignment will have a sum of pairs score of 1", {
  testSeqMap <- c(1, 2, 3, 4, 5, NA)
  refSeqMap <- c(1, 2, 3, 4, 5, NA)
  expect_equal(sumPairsScore(refSeqMap, testSeqMap, 6), 1)
})


