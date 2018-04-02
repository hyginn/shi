# test_simulationClosure.R
library(mockery)

context("simulationClosure")

test_that("it should throw a warning if there are no pseudo counts added when
          using kl", {
  msg <- "Setting pseudoCountsValue to non zero to prevent zero frequencies!"
  expect_warning(simulationClosure(1,
                                   refDistribution = rep(1, 4) / 4,
                                   entropyMethod = "kl",
                                   pseudoCountsValue = 0), msg)
})

test_that("it should sample from the right alphabet", {
  mockSample <- mock(c("A", "T", "C", "G"), c("V", "V", "V", "V"))
  stub(simulationClosure, "sample",  mockSample)
  sim1 <- simulationClosure(1,
                            isAminoAcid = FALSE,
                            refDistribution = rep(1, 4) / 4,
                            entropyMethod = "shannon")
  sim2 <- simulationClosure(1,
                            isAminoAcid = TRUE,
                            refDistribution = rep(1, 20) / 20,
                            entropyMethod = "shannon")
  sim1(1)
  sim2(1)
  args <- mock_args(mockSample)
  expect_equal(length(args[[1]][[1]]), 4)
  expect_equal(length(args[[2]][[1]]), 20)
})

test_that("it should simulate properly", {
  equiProb <- rep(1, 4) / 4
  maxInfo <- calcMaxInformation(isAminoAcid = FALSE)
  sim <- simulationClosure(10,
                           isAminoAcid = FALSE,
                           refDistribution = equiProb,
                           entropyMethod = "shannon")
  set.seed(123)
  expectedSimulations <- unlist(lapply(1:10, function(unused) {
    obs <- sample(c("A", "T", "C", "G"),
                  size = 5,
                  prob = equiProb,
                  replace = TRUE)
    freqs <- getFrequencies(obs, isAminoAcid = FALSE)
    return(calcInformation(freqs,
                           isAminoAcid = FALSE,
                           entropyMethod = "shannon",
                           refDistribution = equiProb))
  }))
  set.seed(123)
  simulations <- sim(5)
  expect_equal(length(simulations), 10)
  expect_equal(simulations, expectedSimulations)
  expect_equal(sum(simulations < 0), 0)
  expect_equal(sum(simulations > maxInfo), 0)
})
