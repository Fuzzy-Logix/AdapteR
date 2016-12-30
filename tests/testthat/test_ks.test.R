set.seed(100)                                       
Renv = new.env(parent = globalenv())
Renv$p <- rnorm(50)
Renv$q <- runif(30)
FLenv = as.FL(Renv)

test_that("Kolmogorov-Smirnov Test 2S, exact=FALSE",{
    result = eval_expect_equal({
        a <- ks.test(p, q, exact = FALSE)
    },Renv,FLenv,
    expectation = c("a"),
    check.attributes=T,
    tolerance = .0001
    )
}
)

sqlstr <- paste0("SELECT * FROM tblKSTest")
mt <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())
Renv$m <- mt$NUM_VAL[mt$GROUPID == 1]
Renv$n <- mt$NUM_VAL[mt$GROUPID == 2]
FLenv = as.FL(Renv)
test_that("Kolmogorov-Smirnov Test 2S, exact=FALSE -- DBLytix Example ",{
    result = eval_expect_equal({
        a <- ks.test(m,n,exact = FALSE)
    },Renv,FLenv,
    expectation = c("a"),
    check.attributes= T,
    tolerance = .0001
    )
}
)



