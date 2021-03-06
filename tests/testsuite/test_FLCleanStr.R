## Function FLCleanStr has no equivalent in R .
## these tests are just testing non-changes
## todo: add a test for correct removal of a character

test_that("Check for FLCleanStr function",{
    tryCatch(widetable  <- FLTable(getTestTableName("tblstringID"),"stringID"),
        error=function(e)
            stop("FLTable could not be created. Subsequent tests may fail \n "))
    flv <- widetable[1:6,"string"]
    resultflvector <- FLCleanStr(flv)
    expect_equal(as.R(flv),as.R(resultflvector))
    ##
    widetable  <- FLTable(getTestTableName("tblAutoMpg"),"ObsID")
    flv <- widetable[1:6,"CarName"]
    resultflvector <- FLCleanStr(flv)
    expect_equal(as.R(flv),as.R(resultflvector))
})
