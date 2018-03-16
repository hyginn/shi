# test_calcHeights.R

context("calcHeights")

test_that("Calculate the heights from frequencies and information", {
  # for col c("A", "A", "T", "C")
  freqs <- numeric(4)
  names(freqs) <- c("A", "T", "C", "G")
  freqs["A"] <- 0.5
  freqs["T"] <- 0.25
  freqs["C"] <- 0.25
  freqs["G"] <- 0
  info <- 4
  heights <- calcHeights(freqs, info)
  expect_equal(as.numeric(heights["A"]), 2)
  expect_equal(as.numeric(heights["T"]), 1)
  expect_equal(as.numeric(heights["C"]), 1)
  expect_equal(as.numeric(heights["G"]), 0)
})