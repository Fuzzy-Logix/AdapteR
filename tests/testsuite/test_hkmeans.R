## Test Cases doesn't runs on Aster
## Asana Ticket: https://app.asana.com/0/136555696724838/371726752253556
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
    flt <- FLTable(getTestTableName("tblUSArrests"), 
                    "ObsID", "VarID", "Num_Val")
    hkmeansobj <- hkmeans(flt, 2, 2, 20, 2)
})
