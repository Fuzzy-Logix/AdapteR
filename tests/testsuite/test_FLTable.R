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

##options(debugSQL=TRUE)
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
})

test_that("typeof: FLTable and columns]",{
    result = eval_expect_equal({
        ts <- typeof(irisdata)
        tsc <- typeof(irisdata[,"SepalLength"])
    },
    Renv,FLenv,check.attributes=FALSE)
})

test_that("Selection of columns works with $ and with [,name]",{
    ## A remote matrix is easily created by specifying
    ## table, row id, column id and value columns
    DfilmF <- FLTable(table        = "FL_DEMO.actressldist",
                      obs_id_colname    = "ObsID")
    expect_equal(as.vector(head(DfilmF$Actor)),
                 as.vector(head(DfilmF[,"Actor"])))
})
