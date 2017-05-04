

##
####GARCH Example.
##rv <- sqlQuery(connection, "SELECT stockreturn FROM tblbac_return")
##flv <- as.FL(rv$stockreturn)
##flmod <- garch.FLVector(flv, order = c(1,1))
##
##FLexpect_equal(flmod$Alpha,0.9903334,tolerance = .01 )
##FLexpect_equal(flmod$AIC, -2011.4999,tolerance = .01 )
##FLexpect_equal(flmod$SBC,-2003.05484,tolerance = .01 )
##FLexpect_equal(flmod$Variance,0.0009193735, tolerance = .01)
##
##FLexpect_equal(names(flmod), c("ConvCrit",
##                               "Variance",
##                               "SBC",
##                               "AIC",
##                               "Omega",
##                               "Gamma",
##                               "Beta",
##                               "Alpha"))
##

##ARCHqUDT Example.
rv <- sqlQuery(connection, "SELECT stockprice  FROM tblbac")
flv <- as.FL(rv$stockprice)
flmod <- garch.FLVector(flv, order = c(0,1))

FLexpect_equal(flmod$Alpha,0.9956,tolerance = .01 )
FLexpect_equal(flmod$AIC, 4624.5320,tolerance = .01 )
FLexpect_equal(flmod$SBC,4632.9811,tolerance = .01 )
FLexpect_equal(flmod$Variance,1.0098, tolerance = .01)

FLexpect_equal(names(flmod), c("ConvCrit",
                               "Variance",
                               "SBC",
                               "AIC",
                               "Omega",
                               "Gamma",
                               "Beta",
                               "Alpha"))



## IGARCH Example
rv <- sqlQuery(connection, "SELECT stockreturn FROM tblbac_return")
flv <- as.FL(rv$stockreturn)
flmod <- garch.FLVector(flv, order = c(1,1), type = "Integrated")
FLexpect_equal(flmod$Alpha,0.9903334,tolerance = .01 )
FLexpect_equal(flmod$AIC, -2011.4999,tolerance = .01 )
FLexpect_equal(flmod$SBC,-2003.05484,tolerance = .01 )
FLexpect_equal(flmod$Variance,0.0009193735, tolerance = .01)

FLexpect_equal(names(flmod), c("ConvCrit",
                               "Variance",
                               "SBC",
                               "AIC",
                               "Omega",
                               "Gamma",
                               "Beta",
                               "Alpha"))
