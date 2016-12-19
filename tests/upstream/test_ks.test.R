                                        # Problems with data.name,
                                        # p.value error computed is wrong.
                                        # Kolmogorov-Smirnov (KS) Test 1S
sqlstr <- paste0("SELECT * FROM tblKSTest WHERE GroupID = 1")
t <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())
Renv$a <- t$NUM_VAL
FLenv = as.FL(Renv)
test_that("Kolmogorov-Smirnov Test 1S: Testing DBLytix Example ",{
    result = eval_expect_equal({
        res_exact <- ks.test(a,'pnorm', 3.5, 11.5,exact=TRUE)
        res_nonexact <- ks.test(a,'pnorm', 3.5, 11.5,exact=FALSE)
    },Renv,FLenv,
    expectation = c("q"),
    check.attributes=F,
    tolerance = .0001,
    verbose = T
    )
}
)

                                        # Kolmogorov-Smirnov (KS) Test 1S
                                        # gives p.value as > .25
set.seed(250)
Renv = new.env(parent = globalenv())
Renv$x <- rnorm(10, 1, 2)
FLenv <- as.FL(Renv)
test_that("Kolmogorov-Smirnov Test 1s:", {
    result = eval_expect_equal({
        res_nonexact <- ks.test(x, 'pnorm', 1, 2, exact = FALSE)
        res_exact <- ks.test(x, 'pnorm', 1, 2, exact = TRUE)
    },Renv,FLenv,
    expectation = c("res_exact","res_nonexact"),
    check.attributes = T,
    tolerance = .0001,
    verbose = FALSE
    )
    })

                                        # Kolmogorov-Smirnov (KS) Test 2S
set.seed(100)                                       
Renv = new.env(parent = globalenv())
Renv$p <- rnorm(50)
Renv$q <- runif(30)
FLenv = as.FL(Renv)

test_that("Kolmogorov-Smirnov Test 2S, exact",{
    result = eval_expect_equal({
        a <- ks.test(p, q, exact = TRUE)
    },Renv,FLenv,
    expectation = c("a"),
    check.attributes=F,
    tolerance = .0001,
    verbose = FALSE
    )
}
)

                                        # Kolmogorov-Smirnov (KS) Test 2S

sqlstr <- paste0("SELECT * FROM tblKSTest")
mt <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())

Renv$m <- mt$NUM_VAL[mt$GROUPID == 1]
Renv$n <- mt$NUM_VAL[mt$GROUPID == 2]
FLenv = as.FL(Renv)

test_that("Kolmogorov-Smirnov Test 2S, exact -- DBLytix Example ",{
    result = eval_expect_equal({
        a <- ks.test(m,n)
    },Renv,FLenv,
    expectation = c("a"),
    check.attributes= T,
    tolerance = .0001,
    verbose = FALSE
    )
}
)








