
test_that("FLSimpleVector: values can be stored with store",{
    flvec <- store(runif(FLSerial(10),-1,1))
    FLexpect_equal(mean(flvec), mean(as.R(flvec)))
})



test_that("FLSimpleVector: aggregates by partition",{
})

