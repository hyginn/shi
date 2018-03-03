#test_balibaseTcScore.R

context("balibaseTcScore")

# ==== BEGIN SETUP AND PREPARE =================================================
#

readTypeAA = "AAStringSet"
readTypeDNA = "DNAStringSet"

#example files are MSA in text files in fasta format
score2 <- balibaseTcScore("example2.txt", "example2ref.txt", readTypeDNA)
#score3 <- balibaseTcScore("example3.txt", "example3ref.txt", readTypeAA)

#
# ==== END SETUP AND PREPARE ===================================================

test_that("a sample input prodcues the expected output",  {
  expect_equal(score2, 0.05401112)
  #expect_equal(score3, 0)
})


# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove everything that the test has created, except for stuff in tempdir().
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
