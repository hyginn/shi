# test_calcInformation.R
library(mockery)

context("calcInformation")

freqs <- numeric(4)
names(freqs) <- c("A", "T", "C", "G")
freqs["A"] <- 0.5
freqs["T"] <- 0.25
freqs["C"] <- 0.25
freqs["G"] <- 0.0

test_that("proper max information is used if amino acid is specified", {
  mockCalcMaxInformation <- mock(4, 4)
  stub(calcInformation, 'calcMaxInformation', mockCalcMaxInformation)
  calcInformation(freqs, entropyMethod = "shannon", isAminoAcid = FALSE)
  calcInformation(freqs, entropyMethod = "shannon", isAminoAcid = TRUE)
  args <- mock_args(mockCalcMaxInformation)
  expect_equal(args[[1]][[1]], FALSE)
  expect_equal(args[[2]][[1]], TRUE)
})

test_that("proper entropy method is being used", {
  mockShannonEntropy <- mock(0)
  mockKLdiv <- mock(0)
  stub(calcInformation, "calcShannonEntropy", mockShannonEntropy)
  stub(calcInformation, "calcKLdiv", mockKLdiv)
  expect_called(mockShannonEntropy, 0)
  expect_called(mockKLdiv, 0)
  calcInformation(freqs, entropyMethod = "shannon")
  expect_called(mockShannonEntropy, 1)
  expect_called(mockKLdiv, 0)
  calcInformation(freqs, entropyMethod = "kl")
  expect_called(mockShannonEntropy, 1)
  expect_called(mockKLdiv, 1)
})

test_that("invalid entropy method will be caught", {
  expect_error(calcInformation(freqs, entropyMethod = "unknown"),
               'Unknown entropy method! Use "shannon" or "kl"')
})

# TODO: write integrated tests with mocks
