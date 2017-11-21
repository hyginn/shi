#test_balibaseTcScore.R

context("balibaseTcScore")

# ==== BEGIN SETUP AND PREPARE =================================================
#

readType = "DNAStringSet"

#example2.txt and example2ref.txt are MSA files in fasta format
score <- balibaseTcScore("example2.txt", "example2ref.txt", readType)

#
# ==== END SETUP AND PREPARE ===================================================

test_that("a sample input prodcues the expected output", {
  expect_equal(score, 0.9920572)
})

# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove everything that the test has created, except for stuff in tempdir().
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
