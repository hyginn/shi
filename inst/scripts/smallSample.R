AAref <- numeric()  # Uniprot frequencies October 2017, slightly adjusted to
# sum to 1.0
AAref["A"] <- 0.0904
AAref["C"] <- 0.0123
AAref["D"] <- 0.0545
AAref["E"] <- 0.0617
AAref["F"] <- 0.0394
AAref["G"] <- 0.0724
AAref["H"] <- 0.0221
AAref["I"] <- 0.0573
AAref["K"] <- 0.0504
AAref["L"] <- 0.0986
AAref["M"] <- 0.0240
AAref["N"] <- 0.0392
AAref["P"] <- 0.0486
AAref["Q"] <- 0.0381
AAref["R"] <- 0.0570
AAref["S"] <- 0.0673
AAref["T"] <- 0.0558
AAref["V"] <- 0.0686
AAref["W"] <- 0.0129
AAref["Y"] <- 0.0294


smallSampleCorrection(1, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(2, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(3, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(4, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(5, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(6, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(7, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(8, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(9, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)
smallSampleCorrection(10, isAminoAcid = TRUE, simulated = TRUE, refDistribution = AAref)


library(microbenchmark)
library(ggplot2)

sim1 <- simulationClosure(10000, TRUE, names(AAref), AAref, "kl")
sim2 <- looped(10000, TRUE, names(AAref), AAref, "kl")
mbm <- microbenchmark("lapply" = {b <- sim1(10)},
                      "loop" = {b <- sim2(10)})
mbm
autoplot(mbm)


a <- matrix(0, nrow=10, ncol=100)
z <- list()
for (i in 1:10) {
  z[[i]] <- rep(0, 100)
}

ok1 <- function() {
  lol <- a[4,]
  return(lol)
}

ok2 <- function() {
  lol <- z[[2]]
  return(lol)
}

mbm <- microbenchmark("mat" = {b <- ok1()},
                      "lis" = {b <- ok2()})
mbm
autoplot(mbm)

ye <- smallSampleCorrectionClosure(10)
oki <- smallSampleCorrectionClosure(10, isAminoAcid = TRUE, TRUE, "kl", AAref, TRUE)
