# test_sequenceLogoR.R

context("sequenceLogoR")

test_that("should stop if isAminoAcid is not supplied", {
  expect_error(sequenceLogoR(), "isAminoAcid is not supplied!")
})

test_that("should stop if trying to show gap info without calculating the
          corrections", {
  expect_error(sequenceLogoR(isAminoAcid = TRUE,
                             displayGapInfo = TRUE,
                             calcCorrection = FALSE),
               "Can not show gap info without calculating correction!")
})

test_that("should stop if the reference distribution isn't a named vector", {
  expect_error(sequenceLogoR(isAminoAcid = TRUE,
                             refDistribution = rep(1, 20) / 20),
               "Feed in a named vector for the reference distribution!")
})