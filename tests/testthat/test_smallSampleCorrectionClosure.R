# test_smallSampleCorrectionClosure.R
library(mockery)

context("smallSampleCorrectionClosure")

test_that("a simulation closure will be generated if the small sample,
          corrections are set to use simulations", {
  mockSimulationClosure <- mock()
  stub(smallSampleCorrectionClosure, "simulationClosure", mockSimulationClosure)
  expect_called(mockSimulationClosure, 0)
  smallSampleCorrectionClosure(1, simulate = FALSE,
                               refDistribution = rep(1, 4) / 4)
  expect_called(mockSimulationClosure, 0)
  smallSampleCorrectionClosure(1, simulate = TRUE,
                               refDistribution = rep(1, 4) / 4)
  expect_called(mockSimulationClosure, 1)
})

test_that("it should calculate the correction properly for simulations", {
  eProb <- rep(1, 4) / 4
  mockSimulation <- function(numObservedSamples) {return(1:10)}
  mockSimulationClosure <- mock(mockSimulation)
  stub(smallSampleCorrectionClosure, "simulationClosure", mockSimulationClosure)
  smallSampleCorrection <- smallSampleCorrectionClosure(10,
                                                        simulate = TRUE,
                                                        refDistribution = eProb,
                                                        pseudoCountsValue = 0.1)
  hypotheticalInfo <- 5.5
  expectedCorrectionFactor <- sum(hypotheticalInfo <= 1:10) / 10
  expectedCorrectedInfo <- 5.5 * expectedCorrectionFactor
  correctedInfo <- smallSampleCorrection(10, hypotheticalInfo)
  expect_equal(correctedInfo, expectedCorrectedInfo)
})

test_that("it should calculate the correction properly for simple corrections",
          {
  smallSampleCorrection <- smallSampleCorrectionClosure(10,
                                                        isAminoAcid = FALSE,
                                                        simulate = FALSE)
  numObserved <- 5
  hypotheticalInformation <- 3
  expectedCorrection <- 3 / (log(2) * 2 * 5)
  expectedCorrectedInfo <- hypotheticalInformation - expectedCorrection
  correctedInfo <- smallSampleCorrection(numObserved, hypotheticalInformation)
  expect_equal(correctedInfo, expectedCorrectedInfo)
})

test_that("it should use cache when appropriate", {
  eProb <- rep(1, 4) / 4
  mockSimulation <- mock(rep(1, 10), 1:10)
  mockSimulationClosure <- mock(mockSimulation)
  log2 <- log(2)
  mockLog <- mock(log2, log2)
  stub(smallSampleCorrectionClosure, "simulationClosure", mockSimulationClosure)
  stub(smallSampleCorrectionClosure, "log", mockLog)
  smallSampleCorrection1 <- smallSampleCorrectionClosure(
                              10, isAminoAcid = FALSE, simulate = TRUE,
                              refDistribution = eProb, pseudoCountsValue = 0.1)
  smallSampleCorrection2 <- smallSampleCorrectionClosure(
                              10, isAminoAcid = FALSE, simulate = FALSE)
  expect_called(mockSimulation, 0)
  expect_called(mockLog, 0)
  corrected1 <- smallSampleCorrection1(4, 4)
  expect_called(mockSimulation, 1)
  expect_called(mockLog, 0)
  corrected2 <- smallSampleCorrection1(4, 4)
  expect_equal(corrected1, corrected2)
  expect_called(mockSimulation, 1)
  expect_called(mockLog, 0)
  corrected3 <- smallSampleCorrection1(5, 4)
  expect_false(corrected2 == corrected3)
  expect_called(mockSimulation, 2)
  expect_called(mockLog, 0)
  smallSampleCorrection2(1, 1)
  expect_called(mockSimulation, 2)
  expect_called(mockLog, 1)
  smallSampleCorrection2(1, 1)
  expect_called(mockSimulation, 2)
  expect_called(mockLog, 1)
  smallSampleCorrection2(6, 1)
  expect_called(mockSimulation, 2)
  expect_called(mockLog, 2)
})

test_that("corrected information should decrease with lower amount of observed
          samples", {
  eProb <- rep(1, 4) / 4
  vectorIsDecreasingEqual <- function(vectorToCheck) {
    i <- 2
    while (i <= length(vectorToCheck)) {
      if (vectorToCheck[i] > vectorToCheck[i - 1]) {
        return(FALSE)
      }
      i <- i + 1
    }
    return(TRUE)
  }
  performCorrection <- function(smallSampleCorrectionFunc) {
    return(unlist(10:1, function(numObserved) {
      return(smallSampleCorrectionFunc(numObserved, 3))
    }))
  }
  smallSampleCorrection1 <- smallSampleCorrectionClosure(
                              10, isAminoAcid = FALSE, simulate = TRUE,
                              refDistribution = eProb, pseudoCountsValue = 0.1)
  smallSampleCorrection2 <- smallSampleCorrectionClosure(
                              10, isAminoAcid = FALSE, simulate = FALSE)
  set.seed(321)
  correctionSet1 <- performCorrection(smallSampleCorrection1)
  correctionSet2 <- performCorrection(smallSampleCorrection2)
  expect_true(vectorIsDecreasingEqual(correctionSet1))
  expect_true(vectorIsDecreasingEqual(correctionSet2))
})
