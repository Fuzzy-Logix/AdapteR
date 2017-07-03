##Anderson-Darling Test:Test Case 1

Renv = new.env(parent = globalenv())
set.seed(200)
Renv$a <- rnorm(100, mean = 5, sd = 3)
FLenv = as.FL(Renv)

test_that("Anderson-Darling Test: nortest package example", {
    result = eval_expect_equal({
        res <- ad.test(a)
        statistic <- res$statistic
        pValue <- res$p.value
    },Renv,FLenv,
    expectation = c("statistic", "pValue"),
    noexpectation = "res",
    check.attributes = T,
    tolerance = .01,
    scale = 1,
    verbose = T
    )
})


#Anderson-Darling Test:Test Case 2 
data <- FLTable(table = getTestTableName("tblADTest"), obs_id_colname="ObsID",whereconditions="datasetid=1")
val <- data$NUM_VAL


test_that("Anderson-Darling Test: DBLytix Example", {   
    res <- ad.test(val, 1, 10)
    statistic <- res$statistic
    pValue <- res$p.value
    expect_equal(statistic, 1,257.496, check.attributes = FALSE)
    expect_equal(pValue, 0.001) 
})


