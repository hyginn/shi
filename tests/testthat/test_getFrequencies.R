# test_getFrequences.R

context("getFrequencies")

test_that("there will be a panic when the input column doesn't match
          isAminoAcid flag", {
    currCol <- c("K", "I", "A")
    expect_error(getFrequencies(currCol, isAminoAcid = FALSE),
                 "PANIC: unknown residue!")
})

test_that("frequencies will be calculated correctly", {
  currCol <- c("A", "A", "C", "G")
  freqs <- getFrequencies(currCol, isAminoAcid = FALSE)
  expectedFreqs <- as.numeric(c(2, 0, 1, 1))
  names(expectedFreqs) <- c("A", "T", "C", "G")
  expectedFreqs <- expectedFreqs / sum(expectedFreqs)
  expect_equal(freqs, expectedFreqs)
})

test_that("gap chracters will be ignored", {
  currCol <- c("A", "T", "C", "-", "G")
  freqs <- getFrequencies(currCol, isAminoAcid = FALSE, gapCharacter = "-")
  expectedFreqs <- as.numeric(c(1, 1, 1, 1))
  names(expectedFreqs) <- c("A", "T", "C", "G")
  expectedFreqs <- expectedFreqs / sum(expectedFreqs)
  expect_equal(freqs, expectedFreqs)
})

test_that("pseudo counts will prevent 0 freqs", {
  currCol <- c("A", "A", "C", "G")
  freqs1 <- getFrequencies(currCol, isAminoAcid = FALSE, pseudoCountsValue = 0)
  freqs2 <- getFrequencies(currCol, isAminoAcid = FALSE, pseudoCountsValue = 0.1)
  expect_equal(sum(freqs1 == 0), 1)
  expect_equal(sum(freqs2 == 0), 0)
})
