Renv = new.env(parent = globalenv())

Renv$irisdata <- iris
Renv$testdf <- data.frame(mylogic=c(TRUE,FALSE,TRUE),
                          myinteg=1:3,
                          myfloat=1:3/3,
                          myfact=as.factor(c("a","b","a")),
                          mychar=c("one","two","three"))
                          
colnames(Renv$irisdata) <- gsub("\\.","",colnames(Renv$irisdata),fixed = FALSE)
FLenv <- as.FL(Renv)

options(debugSQL=TRUE)
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
    ##print(result)
})

options(debugSQL=TRUE)
test_that("FLTable supports different types",{
    FLexpect_equal(FLenv$testdf,Renv$testdf)
    print(result)
})


FLenv$testdf
