Renv = new.env(parent = globalenv())
FLenv <- as.FL(Renv)
Renv$irisdata <- iris
Renv$testdf <- data.frame(mylogic=c(TRUE,FALSE,TRUE),
                          myinteg=1:3,
                          myfloat=1:3/3,
                          myfact=as.factor(c("a","b","a")),
                          mychar=c("one","two","three"))
rownames(Renv$irisdata) <- 1:nrow(Renv$irisdata)
colnames(Renv$irisdata) <- gsub("\\.","",colnames(Renv$irisdata),fixed = FALSE)

FLenv$irisdata <- as.FLTable.data.frame(Renv$irisdata,temporary=FALSE)
FLenv$testdf <- as.FLTable.data.frame(Renv$testdf,temporary=FALSE)

test_that("FLTable dims and names",{
    result = eval_expect_equal({ 
        trows <- nrow(irisdata)
        tdnames <- dimnames(irisdata)
        },
    Renv,FLenv,check.attributes=FALSE)
})

##options(debugSQL=TRUE)
test_that("FLTable in-database transformations, type double -- ALTER TABLE, adding new columns",{
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

## typeof for FLTable is different by design
test_that("typeof: FLTable and columns",{
    result = eval_expect_equal({
        #ts <- typeof(irisdata)
        tsc <- typeof(irisdata[,"SepalLength"])
    },
    Renv,FLenv,check.attributes=FALSE)
})

if(is.TD())
test_that("as.dta.frame: download (part) of a remote table",{
    ## A remote matrix is easily created by specifying
    ## table, row id, column id and value columns
    DfilmF <- FLTable(table        = "FL_DEMO.actressldist",
                      obs_id_colname    = "obsid")
    DfilmR <- as.data.frame(DfilmF)
    expect_equal(nrow(DfilmF), nrow(DfilmR))
})


if(is.TD())
test_that("Selection of columns works with $ and with [,name]",{
    ## A remote matrix is easily created by specifying
    ## table, row id, column id and value columns
    DfilmF <- FLTable(table        = "FL_DEMO.actressldist",
                      obs_id_colname    = "obsid")
    colnames(DfilmF) <- tolower(colnames(DfilmF))
    expect_equal(as.vector(head(DfilmF$actor)),
                 as.vector(head(DfilmF[,"actor"])))
})

#######################
#### test cases of wideToDeep
test_that("check examples from DB-Lytix manual runs:: FLWideToDeep",{
    widetable  <- FLTable(getTestTableName("tblAbaloneWide"), "ObsID", , whereconditions= "obsID< 101")
    deeptable <- wideToDeep(widetable, ExcludeCols= "Sex", classSpec= "DummyCat(D)")
    RDeepTable <- as.R(deeptable)

    ## check dimension of deeptable obtained
    # from deeptable obsID col removed(-1), sex col removed(-1) and 2 cols of DummyCat added.
    FLexpect_equal(deeptable@dims, dim(widetable)+c(0,1))
    })

########################
###### test case of deepToWide
test_that("check dimension of wideTable generated",{
    deeptable  <- FLTable(getTestTableName("tblUSArrests"), "ObsID","VarID","Num_Val")
    resultList <- deepToWide(deeptable)
    widetable <- resultList$table

    ## check dimension of widetable
    FLexpect_equal(dim(widetable), dim(deeptable))
    })

########################
#### test cases of FLRegrDataPrep

test_that("check FLRegrDataPrep output deeptable dimensions",{
    FLiris <- FLTable("iris","obsid")
    irisDeep <- FLRegrDataPrep(FLiris, "petallength")
    #column size of irisDeep increases by 1 because of conversion
    #of categorical variable to dummy variable.
    FLexpect_equal(irisDeep@dims, dim(FLiris) + c(0,1), platforms= c("TD", "Hadoop"))
    #check cateogorical variable are correctly converted to dummy variable.
    v= c(-1)
    for(i in 0:(irisDeep@dims[2]-2)){
        v<- c(v, i)  
    }
    FLexpect_equal(colnames(irisDeep),v, platforms= c("TD", "Hadoop"))
})

test_that("check ExcludeCols parameter of FLRegrDataPrep",{
    widetable  <- FLTable(getTestTableName("tblAutoMPG"),
                         "ObsID", whereconditions= "ObsID <101")
    colnames(widetable) <- tolower(colnames(widetable))
    deeptable <- FLRegrDataPrep(widetable,"mpg", ExcludeCols= "carname")
    #dimension of widetable and deeptable will be same
    #Because categorical variable "CarName" is excluded from conversion.
    FLexpect_equal(deeptable@dims, dim(widetable), platforms= c("TD", "Hadoop"))
})

#### Running examples from the DBlytix Manual
test_that("check examples from DBLytix manual: FLRegrDataPrep runs",{
    widetable  <- FLTable(getTestTableName("tblAutoMPG"),
                         "ObsID", whereconditions= "ObsID <101")
    colnames(widetable) <- tolower(colnames(widetable))
    deeptable <- FLRegrDataPrep(widetable,"mpg")
    analysisID <- deeptable@wideToDeepAnalysisID 
    deeptableR <- as.R(deeptable)

    widetableTest <- FLTable(getTestTableName("tblAutoMPGWideTest"),
                             "ObsID", whereconditions= "ObsID <101")
    deeptableTest <- FLRegrDataPrep(widetableTest, TrainOrTest= 1, InAnalysisID= analysisID)
    deeptableTestR <- as.R(deeptableTest)
})
