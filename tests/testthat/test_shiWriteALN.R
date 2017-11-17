# test_shiWriteALN.R
#

context("shiWriteALN")

# ==== BEGIN SETUP AND PREPARE =================================================
#
ref <- character()
ref[1] <- "CLUSTAL W format. Test"
ref[2] <- ""
ref[3] <- "firstSeq   MRMDPVI--MIMLWTARGPPDFVDFDCRNK"
ref[4] <- "secondSeq  MKMDPVVKSLIVIWSAR---AFVQIDCRQQ"
ref[5] <- ""
ref[6] <- ""
ref[7] <- "firstSeq   RGFYNHDMRDASQYFHLE"
ref[8] <- "secondSeq  RGFY--------------"
ref[9] <- ""
ref[10] <- ""

mySeq <- c(firstSeq  = "MRMDPVI--MIMLWTARGPPDFVDFDCRNKRGFYNHDMRDASQYFHLE",
           secondSeq = "MKMDPVVKSLIVIWSAR---AFVQIDCRQQRGFY")

fN <- tempfile()
shiWriteALN(mySeq, myCon = fN, note = "Test", blockWidth = 30)

#
# ==== END SETUP AND PREPARE ===================================================


test_that("a sample input prodcues the expected output", {
    expect_equal(readLines(fN), ref)
})


# ==== BEGIN TEARDOWN AND RESTORE ==============================================
# Remove everything that the test has created, except for stuff in tempdir().
#
# ==== END  TEARDOWN AND RESTORE ===============================================

# [END]
