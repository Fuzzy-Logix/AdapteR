
library(randtests)
                                        ##Wald-Wolf 1s Test

sqlstr <- paste0("SELECT * FROM tblWW1SMulti WHERE DATASETID = 1")
res <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())
Renv$dat <- res$num_val
FLenv = as.FL(Renv)
                                        # Making the model.
Renv$res1 <- runs.test(Renv$dat,threshold = 6.2 )
FLenv$res1 <- WaldWolftest1s(FLenv$dat, threshold = 6.2)

test_that("Wald-Wolfowitz One Sample Test:",{
    result = eval_expect_equal({
        p.val <- res1$p.value
        z <- res1$statistic
    },Renv,FLenv,
    noexpectation=c("res1"),
    expectation = c("p.val","z"),
    check.attributes=FALSE)
    })


data(sweetpotato)
Renv = new.env(parent = globalenv())
Renv$yield <- sweetpotato$yield
FLenv = as.FL(Renv)
Renv$res1 <- runs.test(Renv$yield)
FLenv$res1 <- WaldWolftest1s(FLenv$yield)

test_that("Wald-Wolfowitz One Sample Test:",{
    result = eval_expect_equal({
        p.val <- res1$p.value
        z <- res1$statistic
    },Renv,FLenv,
    noexpectation=c("res1"),
    expectation = c("p.val","z"),
    check.attributes=FALSE)
    })
            
                         ##Wald-Wolf 2s Test
tbl1 <- FLTable(getTestTableName("tblWW2SMulti"),
                "obsid",
                 whereconditions= "datasetid=1 and groupid=1")
tbl2 <- FLTable(getTestTableName("tblWW2SMulti"),
                "obsid",
                 whereconditions= "datasetid=1 and groupid=2")
v1 <- tbl1$num_val
v2 <- tbl2$num_val
test_that("Wald-Wolfowitz One Sample Test:",{
    result = WaldWolftest2s(v1, v2)
    FLexpect_equal(as.numeric(result$statistic), 0.605558, tolerance= 1e-5)
    FLexpect_equal(as.numeric(result$p.value), 0.544808, tolerance= 1e-5)
    })