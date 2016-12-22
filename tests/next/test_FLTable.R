Renv = new.env(parent = globalenv())

Renv$irisdata <- iris
Renv$testdf <- data.frame(mylogic=c(TRUE,FALSE,TRUE),
                          myinteg=1:3,
                          myfloat=1:3/3,
                          myfact=as.factor(c("a","b","a")),
                          mychar=c("one","two","three"))

rownames(Renv$irisdata) <- 1:nrow(Renv$irisdata)
colnames(Renv$irisdata) <- gsub("\\.","",colnames(Renv$irisdata),fixed = FALSE)
FLenv <- as.FL(Renv)

irisdata <- FLenv$irisdata

test_that("FLTable in-database transformations, type logical -- UPDATE existing columns",{
    result = eval_expect_equal({ 
        irisdata$SepalLongerThanWide2 <- irisdata$SepalLength > irisdata$SepalWidth*2
        test2 <- irisdata$SepalLongerThanWide2
        },
    Renv,FLenv,check.attributes=FALSE)
})

test_that("FLTable dims and names",{
    result = eval_expect_equal({ 
        tdims <- dim(irisdata)
        tcols <- ncol(irisdata)
        },
    Renv,FLenv,check.attributes=FALSE)
})
