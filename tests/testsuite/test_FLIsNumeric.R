


test_that("Check correct result for FLIsNumeric ",{
    vecChar <- letters[3:1]
    vecInt <- c("1","2")
    vecFloat <- c("1","2.2")
    castExpectIsNumeric <- function(x)
        FLexpect_equal(FLIsNumeric(as.FL(x)), !is.na(as.numeric(x)))
    castExpectIsNumeric(vecChar)
    castExpectIsNumeric(vecInt)
    castExpectIsNumeric(vecFloat)
})


test_that("Check for FLIsNumeric function",{
    widetable  <- FLTable(getTestTableName("tblAutoMpg"),"ObsID")
    flv <- widetable[1:6,"CarName"]
    resultflvector <- FLIsNumeric(flv)
})
