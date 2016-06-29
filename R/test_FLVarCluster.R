library(testthat)

#asana ticket           https://app.asana.com/0/143316600934101/149556758745183
deeptable  <- FLTable( "fuzzylogix", "tblLogRegr", "ObsID","VarID","Num_Val")
test_that("FLVarCluster",
          clustervector <- FLVarCluster(deeptable,0.75,"COVAR",whereconditions=" VarID>0 ")
)
