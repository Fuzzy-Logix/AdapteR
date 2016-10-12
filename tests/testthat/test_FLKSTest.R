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
        q <- ks.test(a,'pnorm', 3.5, 11.5)
#        class(q) <- "list"
    },Renv,FLenv,
    expectation = c("q"),
    check.attributes=F,
    tolerance = .0001,
    verbose = FALSE
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
        val <- ks.test(x, 'pnorm', 1, 2, exact = TRUE)
    },Renv,FLenv,
    expectation = "val",
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

test_that("Kolmogorov-Smirnov Test 2S: R Example ",{
    result = eval_expect_equal({
        a <- ks.test(p, q, exact = FALSE)
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
t <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())

Renv$m <- t$NUM_VAL[t$GROUPID == 1]
Renv$n <- t$NUM_VAL[t$GROUPID == 2]
FLenv = as.FL(Renv)

test_that("Kolmogorov-Smirnov Test 2S:DBLytix Example ",{
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








