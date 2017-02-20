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
                nrow(FLenv$data))
    ## two levels and 3 centers => 3^2
    ## 2 excludeCols and 1 obsID
    FLexpect_equal(dim(FLenv$hkmeansobject$centers),
                    c(Ncenters^Nlevels,ncol(FLenv$data)-3))
    FLexpect_equal(length(FLenv$hkmeansobject$tot.withinss),
                    1)
    FLexpect_equal(length(FLenv$hkmeansobject$withinss),
                    Ncenters^Nlevels)
    FLexpect_equal(length(FLenv$hkmeansobject$size),
                    Ncenters^Nlevels)
    FLexpect_equal(dim(FLenv$hkmeansobject$mapping),
                    c(ncol(FLenv$data)-3,3))
})


