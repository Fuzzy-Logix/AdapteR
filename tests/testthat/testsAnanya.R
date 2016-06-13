##Testing FLExpLog.R
test_that("Check FLExpLog.R. Sort, sqrt and Order also.",{
    flm <- FLMatrix("fuzzylogix", "tblMatrixMulti",5, "Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    flv <- as.FLVector((flm))
    rv <- as.vector(flm)
                                        #matricesLog
    FLexpect_equal(log(flm),log(as.matrix(flm)),check.attributes=FALSE)
    FLexpect_equal(log(flm, base=5),log(as.matrix(flm), base = 5),check.attributes=FALSE)
    FLexpect_equal(log2(flm),log2(as.matrix(flm)),check.attributes=FALSE)
    FLexpect_equal(log1p(flm),log1p(as.matrix(flm)),check.attributes=FALSE)
    FLexpect_equal(log10(flm),log10(as.matrix(flm)),check.attributes=FALSE)
    FLexpect_equal(logb(flm, base = 4),logb(as.matrix(flm), base = 4),check.attributes=FALSE)
                                        #vectorLog
    FLexpect_equal(log(flv),log(rv),check.attributes=FALSE)
    FLexpect_equal(log(flv, base=5),log(rv, base = 5),check.attributes=FALSE)
    FLexpect_equal(log2(flv),log2(rv),check.attributes=FALSE)
    FLexpect_equal(log1p(flv),log1p(rv),check.attributes=FALSE)
    FLexpect_equal(log10(flv),log10(rv),check.attributes=FALSE)
    FLexpect_equal(logb(flv, base = 4),logb(rv, base = 4),check.attributes=FALSE)
                                        #matricesExp
    FLexpect_equal(exp(flm),exp(as.matrix(flm)),check.attributes=FALSE)
    FLexpect_equal(expm1(flm),expm1(as.matrix(flm)),check.attributes=FALSE)
                                        #vectorExp
    FLexpect_equal(exp(flv),exp(rv),check.attributes=FALSE)
    FLexpect_equal(expm1(flv),expm1(rv),check.attributes=FALSE)
                                        #sqrt
    FLexpect_equal(sqrt(flv),sqrt(rv),check.attributes=FALSE)
    FLexpect_equal(sqrt(flm),sqrt(as.matrix(flm)),check.attributes=FALSE)
                                        #sort
    FLexpect_equal(sort.FLVector(flv),sort.int(rv),check.attributes=FALSE)
    FLexpect_equal(sort.FLMatrix(flm),sort.int(as.matrix(flm)),check.attributes=FALSE)
                                        #order
    FLexpect_equal(order(flv), order(rv), check.attributes = FALSE)
    FLexpect_equal(order(flm), order(as.matrix(flm)), check.attributes = FALSE)
})

##Testing FLHeadTail.R
test_that("Check FLHeadTail.R",{
    flt <- FLTable(getOption("ResultDatabaseFL"),"tblUSArrests", "ObsID","VarID","Num_Val", whereconditions = "ObsID<21")
    flm <- FLMatrix("fuzzylogix", "tblMatrixMulti",5, "Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    flv <- as.FLVector((flm))
    rv <- as.vector(flm)
                                        #head
    FLexpect_equal(head(flv), head(rv), check.attributes = FALSE)
    FLexpect_equal(head(flm), head(as.matrix(flm)), check.attributes = FALSE)
    FLexpect_equal(head(flt), head(as.data.frame(flt)), check.attributes = FALSE)
                                        #tail
    FLexpect_equal(tail(flv), tail(rv), check.attributes = FALSE)
    FLexpect_equal(tail(flm), tail(as.matrix(flm)), check.attributes = FALSE)
    FLexpect_equal(tail(flt), tail(as.data.frame(flt)), check.attributes = FALSE)
})
