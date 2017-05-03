## tests case for FLdifferenceUdt.
## DBlytix Example.
rv <- sqlQuery(connection, "SELECT NUM_VAL FROM tblTimeSeriesW1")
flv <- as.FL(rv$NUM_VAL)
flmod <- FLdiff(flv)
FLexpect_equal(length(flmod),45 )
FLexpect_equal(length(flmod), nrow(flv))




## test case for FLRegimeshiftUdt.
vdf <-  sqlQuery(connection, "SELECT Num_Val FROM tblRegimeShift WHERE Groupid = 1 AND Obsid <500")
flv <- as.FL(vdf$Num_Val)
flmod <- regimeshift(flv)
## comparing mean with DBLytix.
FLexpect_equal(flmod$Mean[1], 8.8294, tolerance = .0001)
FLexpect_equal(flmod$Mean[2], 10.05817, tolerance = .0001)

## comparing StdDev with DBLytix.
FLexpect_equal(flmod$StdDev[1],0.3435426 , tolerance = .0001)
FLexpect_equal(flmod$StdDev[2],0.8971050 , tolerance = .0001)

## comparing Probablity with DBLytix.
FLexpect_equal(flmod$Prob[1], 0.08421946, tolerance = .0001)
FLexpect_equal(flmod$Prob[2], 0.91578054, tolerance = .0001)



## tests case for FLEWMA.
## DBlytix Example.
rv <- sqlQuery(connection, "SELECT NUM_VAL FROM tblTimeSeriesW1")
flv <- as.FL(rv$NUM_VAL)
flmod <- FLEWMA(flv)

## comparing Probablity with DBLytix.
FLexpect_equal(length(flmod), 7)
FLexpect_equal(flmod$NumObs,45 )
FLexpect_equal(flmod$Variance, 0.26728, tolerance = .01)
FLexpect_equal(names(flmod),c("NumObs","Lambda","Variance","LogL", "SBC", "AIC", "ConvCrit") )

## tests case for FLPP.
## DBlytix Example.

Renv <- new.env(parent = globalenv())
vdf <- sqlQuery(connection, "SELECT Num_Val FROM tblsensex")
Renv$vdata <- vdf$Num_Val
FLenv <- as.FL(Renv)

eval_expect_equal({
    fit <- PP.test(vdata)
},Renv,FLenv,
expectation = "fit",
tolerance = 2)
