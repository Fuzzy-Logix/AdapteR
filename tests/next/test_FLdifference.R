## tests case for FLdifferenceUdt.
## DBlytix Example.
rv <- sqlQuery(connection, "SELECT NUM_VAL FROM tblTimeSeriesW1")
flv <- as.FL(rv$NUM_VAL)
flmod <- FLdiff(flv)
FLexpect_equal(length(flmod),45 )
FLexpect_equal(length(flmod), nrow(flv))



## tests case for FLPP.
## DBlytix Example.

Renv <- new.env(parent = globalenv())
Renv$vdf <- sqlQuery(connection, "SELECT NUM_VAL FROM tblTimeSeriesW1")
##Renv$vdf <- Renv$vdf$NUM_VAL
FLenv <- as.FL(Renv)

FLenv$fit <- PP.test(FLenv$vdf)
Renv$fit <- PP.test(Renv$vdf)


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
flmod <- FLdiff(flv)

## comparing Probablity with DBLytix.
FLexpect_equal(length(flmod), 7)
FLexpect_equal(flmod$NumObs, 2297)
FLexpect_equal(flmod$Variance, 0.0002737248, tolerance = .0001)
