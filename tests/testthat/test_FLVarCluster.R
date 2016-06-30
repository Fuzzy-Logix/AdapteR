library(testthat)

test_that("FLVarCluster works ",{
    deeptable  <- FLTable( getOption("ResultDatabaseFL"), 
                    "tblLogRegr", 
                    "ObsID",
                    "VarID",
                    "Num_Val",
                    whereconditions=c("ObsID < 1001"))
    clustervector <- FLVarCluster(deeptable,
                                    0.75,
                                    "COVAR",
                                    whereconditions=" VarID>0 ")
})
