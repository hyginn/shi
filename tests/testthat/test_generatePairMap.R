# test_generatePairMap.R

context("generatePairMap")

test_that("with an input of two valid sequences a pair map will be generated", {
  seqA <- c("A", "V", "T", "-", "-", "D")
  seqB <- c("A", "V", "T", "F", "F", "D")
  pairMapOut <- generatePairMap(seqA, seqB)
  expectedPairMapA <- c(1, 2, 3, 6)
  expectedPairMapB <- c(1, 2, 3, NA, NA, 4)
  expect_equal(pairMapOut$pairMapA, expectedPairMapA)
  expect_equal(pairMapOut$pairMapB, expectedPairMapB)
})
