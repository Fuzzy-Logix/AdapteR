Renv = new.env(parent = globalenv())
Renv$testdf <- data.frame(mylogic=c(TRUE,FALSE,TRUE),
                          myinteg=1:3,
                          myfloat=1:3/3,
                          myfact=as.factor(c("a","b","a")),
                          mychar=c("one","two","three"))

## rownames column needs to be removed
## https://app.asana.com/0/143778401455745/140240837628916
##options(debugSQL=TRUE)
test_that("FLTable supports different types",{
    FLexpect_equal(FLenv$testdf,Renv$testdf)
    ##print(result)
})

test_that("as.FLTable is replacing special keywords as column names",
          FLexpect_equal(as.FLTable(longley),longley)
          )


test_that("iris table upload works -- names, use mapping table",{
    irisFL <- as.FLTable(iris)
    expect_equal(
        names(iris),
        names(irisFL),
        check.attributes=FALSE)
})



test_that("FLTable new columns from transformations: numeric",{
    result = eval_expect_equal({ 
        irisdata$SepalArea <- irisdata$SepalLength * irisdata$SepalWidth
        irisdata$SepalBoxLength <- 2 * (irisdata$SepalLength + irisdata$SepalWidth)
    },Renv,FLenv,
    expectation="irisdata")
})

test_that("FLTable new columns from transformations: logical",{
    irisFL <- as.FLTable(iris)
    irisFL$SepalLongerThanWide2 <- irisFL$SepalLength > irisFL$SepalWidth*2
    FLexpect_equal(irisFL$SepalLongerThanWide2, iris$Sepal.Length   > iris$Sepal.Width*2)
})

test_that("kmeans(FLTable): iris dataset, deep-to-wide on subset of wide table ",{
    eval_expect_equal({
        (cl <- kmeans(iris[,2:5], 2))
        ## plot(x, col = cl$cluster)
        ## points(cl$centers, col = 1:2, pch = 8, cex = 2)
        clusterDim <- length(cl$cluster)
        centersDim <- dim(cl$centers)
    },Renv,FLenv,
    expectation=c("clusterDim","centersDim"),
    noexpectation="cl")
})
