library(testthat)

#asana ticket           https://app.asana.com/0/143316600934101/149556758745183
test_that("FLVarCluster works ",
    deeptable  <- FLTable( getOption("ResultDatabaseFL"), 
                    "tblLogRegr", 
                    "ObsID",
                    "VarID",
                    "Num_Val")
    clustervector <- FLVarCluster(deeptable,
                                    0.75,
                                    "COVAR",
                                    whereconditions=" VarID>0 ")
)
