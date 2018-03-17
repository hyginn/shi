# test_smallSampleCorrection

context("smallSampleCorrection")

test_that("corrections decrease as there are more samples", {
  c1 <- smallSampleCorrection(100)
  c2 <- smallSampleCorrection(50)
  c3 <- smallSampleCorrection(10)
  expect_gt(c3, c2)
  expect_gt(c2, c1)
})
