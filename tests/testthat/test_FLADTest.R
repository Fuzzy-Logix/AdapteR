
                                        #Anderson-Darling Test:Test Case 1
Renv = new.env(parent = globalenv())
set.seed(200)
Renv$a <- rnorm(100, mean = 5, sd = 3)
FLenv = as.FL(Renv)

test_that("Anderson-Darling Test:nortest package example", {
    result = eval_expect_equal({
        res <- ad.test(a)
    },Renv,FLenv,
    expectation = "res",
    check.attributes = T,
    tolerance = .0001,
    verbose = F
    )
    })


                                        #Anderson-Darling Test:Test Case 2
FLenv = new.env(parent = globalenv())
data <- FLTable(table ="tblADTest", obs_id_colname="ObsID",whereconditions="datasetid=1")
FLenv$val <- data$NUM_VAL
Renv = as.R(FLenv)

test_that("Anderson-Darling Test: DBLytix Example", {
    result = eval_expect_equal({
        res <- ad.test(val)
        res$alternative <- NULL
    },Renv,FLenv,
    expectation = "res",
    check.attributes = T,
    tolerance = .0001,
    verbose = T
    )
    })


FLenv$res

