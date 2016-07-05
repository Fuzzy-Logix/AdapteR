eqnRtn <- FLMatrix(database          = "FL_DEMO",
                   table_name        = "finEquityReturns",
                   row_id_colname    = "TxnDate",
                   col_id_colname    = "TickerSymbol",
                   cell_val_colname  = "EquityReturn")

eqnRtn <- eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')]
FLenv$eqnRtn <- eqnRtn

## as.matrix fails as non-integer indexes
## Error in i + (!(m.i || i1)) : non-numeric argument to binary operator 
Renv$eqnRtn <- na.omit(as.matrix(FLenv$eqnRtn))


test_that("Correlation of equity returns, low precision, 1e-3",
          eval_expect_equal({
              corER <- cor(eqnRtn)
              ##print(corER)
              dim(eqnRtn)
          }, Renv, FLenv,
          tolerance=1e-3))

test_that("Correlation of equity returns, high precision",
          eval_expect_equal({
              corER <- cor(eqnRtn)
              ##print(corER)
              dim(eqnRtn)
          }, Renv, FLenv,
          expectation="corER"
          ))


## Missing function implementations
test_that("Correlation on longley dataset",{
    eval_expect_equal({
        ## Correlation Matrix of Multivariate sample:
        (Cl <- cor(longley))
        ## Graphical Correlation Matrix:
        symnum(Cl) # highly correlated
        ## Spearman's rho  and  Kendall's tau
        symnum(clS <- cor(longley, method = "spearman"))
        symnum(clK <- cor(longley, method = "kendall"))
        ## How much do they differ?
        i <- lower.tri(Cl)
        cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))
        ## cov2cor() scales a covariance matrix by its diagonal
        ##           to become the correlation matrix.
        cov2cor # see the function definition {and learn ..}
        stopifnot(all.equal(Cl, cov2cor(cov(longley))),
                  all.equal(cor(longley, method = "kendall"),
                            cov2cor(cov(longley, method = "kendall"))))
    }, Renv, FLenv)
    ## TODO: add a better and unique description!
})

test_that("Missing Data, swiss dataset",{
    eval_expect_equal({
        ##--- Missing value treatment:
        C1 <- cov(swiss)
        range(eigen(C1, only.values = TRUE)$values) # 6.19        1921
        ## swM := "swiss" with  3 "missing"s :
        swM <- swiss
        colnames(swM) <- abbreviate(colnames(swiss), min=6)
        swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"
        ## Consider all 5 "use" cases :
        (C. <- cov(swM)) # use="everything"  quite a few NA's in cov.matrix
        try(cov(swM, use = "all")) # Error: missing obs...
        C2 <- cov(swM, use = "complete")
        stopifnot(identical(C2, cov(swM, use = "na.or.complete")))
        range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
        C3 <- cov(swM, use = "pairwise")
        range(eigen(C3, only.values = TRUE)$values) # 6.19   1938
    }, Renv, FLenv)
})

test_that("Swiss dataset, ...",{
    eval_expect_equal({
        ## swM := "swiss" with  3 "missing"s :
        swM <- swiss
        ## Kendall's tau doesn't change much:
        symnum(Rc <- cor(swM, method = "kendall", use = "complete"))
        symnum(Rp <- cor(swM, method = "kendall", use = "pairwise"))
        symnum(R. <- cor(swiss, method = "kendall"))
        ## "pairwise" is closer componentwise,
        summary(abs(c(1 - Rp/R.)))
        summary(abs(c(1 - Rc/R.)))
        ## but "complete" is closer in Eigen space:
        EV <- function(m) eigen(m, only.values=TRUE)$values
        summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))
        0
    }, Renv, FLenv,
    "correlations in R only -- need pulling data out to above setup")
})



## Testing FLCorrel
##Failing because of precision errors.
test_that("check FLCorrel result",
{
    fltDeep <- FLTable(getOption("ResultDatabaseFL"),"tblUSArrests",
                       "ObsID","VarID","Num_Val", whereconditions = "OBsID<21")
    m <- initF.FLMatrix(20)
    flm <- m$FL
    FLexpect_equal(cor(flm,fltDeep),cor(Rm,RtDeep),check.attributes=FALSE)
})
