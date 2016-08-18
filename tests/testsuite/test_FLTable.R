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

##options(debugSQL=TRUE)
test_that("FLTable in-database transformations, type double -- ALTER TABLE, adding new columns",{
    result = eval_expect_equal({ 
        irisdata$SepalArea <- irisdata$SepalLength * irisdata$SepalWidth
        test1 <- irisdata$SepalArea
        irisdata$SepalBoxLength <- 2 * (irisdata$SepalLength + irisdata$SepalWidth)
        test3 <- irisdata$SepalBoxLength
        },
    Renv,FLenv,check.attributes=FALSE)
})

## typeof for FLTable is different by design
test_that("typeof: FLTable and columns",{
    result = eval_expect_equal({
        #ts <- typeof(irisdata)
        tsc <- typeof(irisdata[,"SepalLength"])
    },
    Renv,FLenv,check.attributes=FALSE)
})

test_that("as.dta.frame: download (part) of a remote table",{
    ## A remote matrix is easily created by specifying
    ## table, row id, column id and value columns
    DfilmF <- FLTable(table        = "FL_DEMO.actressldist",
                      obs_id_colname    = "ObsID")
    DfilmR <- as.data.frame(DfilmF)
    expect_equal(nrow(DfilmF), nrow(DfilmR))
})


test_that("Selection of columns works with $ and with [,name]",{
    ## A remote matrix is easily created by specifying
    ## table, row id, column id and value columns
    DfilmF <- FLTable(table        = "FL_DEMO.actressldist",
                      obs_id_colname    = "ObsID")
    expect_equal(as.vector(head(DfilmF$Actor)),
                 as.vector(head(DfilmF[,"Actor"])))
})

