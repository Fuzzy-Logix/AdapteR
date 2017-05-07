## tests case for FLdifferenceUdt.
## DBlytix Example.
rv <- sqlQuery(connection, "SELECT NUM_VAL as num_val FROM tblTimeSeriesW1")
flv <- as.FL(rv$num_val)
flmod <- FLdiff(flv)
FLexpect_equal(length(flmod),45 )
FLexpect_equal(length(flmod), nrow(flv))




## test case for FLRegimeshiftUdt.
vdf <-  sqlQuery(connection, "SELECT Num_Val as num_val FROM tblRegimeShift WHERE Groupid = 1 AND Obsid <500")
flv <- as.FL(vdf$num_val)
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
rv <- sqlQuery(connection, "SELECT NUM_VAL as num_val FROM tblTimeSeriesW1")
flv <- as.FL(rv$num_val)
flmod <- FLEWMA(flv)

## comparing Probablity with DBLytix.
FLexpect_equal(length(flmod), 7)
FLexpect_equal(flmod$numobs,45 )
FLexpect_equal(flmod$variance, 0.26728, tolerance = .01)
FLexpect_equal(names(flmod),
                c("numobs","lambda","variance",
                    "logL", "SBC", "AIC", "ConvergenceCriteria") )

## tests case for FLPP.
## DBlytix Example.
## Results do not match

Renv <- new.env(parent = globalenv())
vdf <- sqlQuery(connection, "SELECT Num_Val as num_val FROM tblsensex")
Renv$vdata <- vdf$num_val
FLenv <- as.FL(Renv)

eval_expect_equal({
    fit <- PP.test(vdata)
    fit$data.name <- NULL
},Renv,FLenv,
noexpectation = "fit",
tolerance = 2)
