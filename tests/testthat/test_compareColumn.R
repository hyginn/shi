#test_compareColumn.R

context("compareColumn")

# ==== BEGIN SETUP AND PREPARE =================================================
#

testCol <- c("A", "C", "C", "G", "G", "A", "T")
refCol <- c("A", "C", "C", "C", "C", "A", "T")

value <- compareColumn(testCol, refCol)

#
# ==== END SETUP AND PREPARE ===================================================

test_that("a sample input prodcues the expected output", {
  expect_equal(value, FALSE)
})

# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove everything that the test has created, except for stuff in tempdir().
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
