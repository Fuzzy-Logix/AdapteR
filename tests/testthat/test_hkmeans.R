library(testthat)
FLenv <- new.env(parent = globalenv())

FLenv$data  <- FLTable(getOption("ResultDatabaseFL"), "tblAutoMpg", "ObsID")
test_that("check HKMeans output dimensions ",{
    centers=3
    levels=2
    FLenv$hkmeansobject <- hkmeans(x=FLenv$data,
                            centers=centers,
                            levels=levels,
                            iter.max=20,
                            nstart=1,
                            excludeCols="CarName,CarNum")
    FLexpect_equal(length(FLenv$hkmeansobject$cluster),
                nrow(FLenv$data))
    ## two levels and 3 centers => 3^2
    ## 2 excludeCols and 1 obsID
    FLexpect_equal(dim(FLenv$hkmeansobject$centers),
                    c(centers^levels,ncol(FLenv$data)-3))
    FLexpect_equal(length(FLenv$hkmeansobject$tot.withinss),
                    1)
    FLexpect_equal(length(FLenv$hkmeansobject$withinss),
                    centers^levels)
    FLexpect_equal(length(FLenv$hkmeansobject$size),
                    centers^levels)
    FLexpect_equal(dim(FLenv$hkmeansobject$mapping),
                    c(ncol(FLenv$data)-3,3))
})