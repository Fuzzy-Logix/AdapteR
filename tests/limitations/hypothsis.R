## Kolmogorov-Smirnov (KS) Test 1S

sqlstr <- paste0("SELECT * FROM tblKSTest WHERE GroupID = 1")
t <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())
Renv$a <- t$NUM_VAL
FLenv = as.FL(Renv)

test_that("Kolmogorov-Smirnov Test 1S: Testing DBLytix Example ",{
    result = eval_expect_equal({
        q <- ks.test(a,'pnorm', 3.5, 11.5)
    },Renv,FLenv,
    expectation = c("q"),
    check.attributes=F,
    tolerance = .01,
    verbose = FALSE
    )
}
)

##Anderson-Darling Test:Test Case 

FLenv = new.env(parent = globalenv())
data <- FLTable(table ="tblADTest", obs_id_colname="ObsID",whereconditions="datasetid=1")
FLenv$val <- data$NUM_VAL
Renv = as.R(FLenv)

test_that("Anderson-Darling Test: DBLytix Example", {
    result = eval_expect_equal({
        res <- ad.test(val)
    },Renv,FLenv,
    expectation = "res",
    check.attributes = T,
    tolerance = .01,
    verbose = T
    )
    })


