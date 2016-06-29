Renv = new.env(parent = globalenv())

Renv$irisdata <- iris
colnames(Renv$irisdata) <- gsub("\\.","",colnames(Renv$irisdata),fixed = FALSE)
FLenv <- as.FL(Renv)

test_that("FLTable in-database transformations work -- ALTER TABLE and UPDATE",{
    result = eval_expect_equal({ 
        irisdata$SepalArea <- irisdata$SepalLength * irisdata$SepalWidth
        test1 <- irisdata$SepalArea
        irisdata$SepalLongerThanWide2 <- irisdata$SepalLength > irisdata$SepalWidth*2
        test2 <- irisdata$SepalLongerThanWide2
        irisdata$SepalBoxLength <- 2 * (irisdata$SepalLength + irisdata$SepalWidth)
        test3 <- irisdata$SepalBoxLength
        },
    Renv,FLenv,check.attributes=FALSE)
    print(result)
})