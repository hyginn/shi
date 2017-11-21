# test_isGap.R

context("isGap")

test_that("isGap works for a single element", {
  expect_equal(isGap("C"), FALSE)
  expect_equal(isGap("-"), TRUE)
})

test_that("isGap works for multiple elements", {
  seqA <- c("A", "V", "T", "-", "-", "D")
  expect_equal(isGap(seqA), c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})
