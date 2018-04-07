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
  calcInformation(freqs, entropyMethod = "kl", refDistribution = rep(1, 4) / 4)
  expect_called(mockShannonEntropy, 1)
  expect_called(mockKLdiv, 1)
})

test_that("invalid entropy method will be caught", {
  expect_error(calcInformation(freqs, entropyMethod = "unknown"),
               'Unknown entropy method! Use "shannon" or "kl"')
})

test_that("an equiprobable distribution will be used for kl if not supplied", {
  mockKLdiv <- mock(0)
  stub(calcInformation, "calcKLdiv", mockKLdiv)
  expect_warning(calcInformation(freqs, entropyMethod = "kl"))
  args <- mock_args(mockKLdiv)
  expect_equal(args[[1]][[2]], rep(1, 4) / 4)
})

test_that("proper information is being returned", {
  mockShannonEntropy <- mock(1)
  mockMaxInformation <- mock(2)
  mockKLdiv <- mock(4)
  stub(calcInformation, "calcShannonEntropy", mockShannonEntropy)
  stub(calcInformation, "calcMaxInformation", mockMaxInformation)
  stub(calcInformation, "calcKLdiv", mockKLdiv)
  expect_called(mockShannonEntropy, 0)
  expect_called(mockMaxInformation, 0)
  expect_called(mockKLdiv, 0)
  shanInfo <- calcInformation(freqs, entropyMethod = "shannon")
  expect_called(mockShannonEntropy, 1)
  expect_called(mockMaxInformation, 1)
  expect_called(mockKLdiv, 0)
  expect_equal(shanInfo, 2 - 1)
  klInfo <- calcInformation(freqs, entropyMethod = "kl",
                            refDistribution = rep(1, 4) / 4)
  expect_called(mockShannonEntropy, 1)
  expect_called(mockMaxInformation, 1)
  expect_called(mockKLdiv, 1)
  expect_equal(klInfo, 4)
})
