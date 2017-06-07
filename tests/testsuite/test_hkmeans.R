library(testthat)
FLenv <- new.env(parent = globalenv())

FLenv$data  <- FLTable(getTestTableName("tblAutoMpg"),"ObsID")
test_that("check HKMeans output dimensions ",{
    Ncenters=3
    Nlevels=2
    FLenv$hkmeansobject <- hkmeans(x=FLenv$data,
                            centers=Ncenters,
                            levels=Nlevels,
                            iter.max=20,
                            nstart=1,
                            excludeCols="CarName,CarNum")
    FLexpect_equal(length(FLenv$hkmeansobject$cluster),
                nrow(FLenv$data), platforms= c("TD"))
    ## two levels and 3 centers => 3^2
    ## 2 excludeCols and 1 obsID
    FLexpect_equal(dim(FLenv$hkmeansobject$centers),
                    c(Ncenters^Nlevels,ncol(FLenv$data)+3), platforms= c("TD"))
    FLexpect_equal(length(FLenv$hkmeansobject$tot.withinss),
                    1, platforms= c("TD"))
    FLexpect_equal(length(FLenv$hkmeansobject$withinss),
                    Ncenters^Nlevels, platforms= c("TD"))
    FLexpect_equal(length(FLenv$hkmeansobject$size),
                    Ncenters^Nlevels, platforms= c("TD"))
    FLexpect_equal(dim(FLenv$hkmeansobject$mapping),
                    c(ncol(FLenv$data)+3,3), platforms= c("TD"))
})

####################
## DBLytix Example
test_that("Testing if basic DBLytix FLHKMeansUdt-hkmeans Example runs from AdapteR",{
    FLMatrixObj <- FLMatrix(getTestTableName("tblmatrixmulti"),5,
                            "matrix_id",
                            "row_id",
                            "col_id",
                            "cell_val",
                            dims= c(5,5))
    ResultFLMatrixObj <- solve(FLMatrixObj)
    
})
