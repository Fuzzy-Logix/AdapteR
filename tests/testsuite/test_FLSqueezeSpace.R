                                        #Not in R .
test_that("FLSqueezeSpace",{
    widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
    flv <- widetable[1:6,"string"]
    resultflvector <- FLSqueezeSpace(flv)
    expect_equal(as.R(flv),as.R(resultflvector))

    singleSpace <- c("DU ANE", "Home Depot", "MARH TA", "WAL MA RT", "WALMART", "WARTHA")
    multiSpace <- gsub(" ","   ",singleSpace)
    flv <- as.FL(multiSpace)
    expect_equal(singleSpace,as.R(resultflvector))
    resultflvector <- FLSqueezeSpace(flv)
    expect_equal(singleSpace,as.R(resultflvector))
})
