# test_calcMaxInformation.R

context("calcMaxInformation")

test_that("calculate the max information correctly", {
  expect_equal(calcMaxInformation(isAminoAcid = FALSE), log2(4))
  expect_equal(calcMaxInformation(isAminoAcid = TRUE), log2(20))
})