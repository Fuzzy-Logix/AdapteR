
                                        #Wald WOlf 2s Test:
sqlstr <- paste0("SELECT * FROM tblWW2SMulti WHERE DATASETID = 1")
res <- sqlQuery(connection, sqlstr)
Renv = new.env(parent = globalenv())

Renv$x <- res$num_val[res$groupid == 1]
Renv$y <- res$num_val[res$groupid == 2]
FLenv = as.FL(Renv)
Renv$res1 <- runs.test(Renv$x, Renv$y, alternative = "two.sided")
FLenv$res1 <- WaldWolftest2s(FLenv$x, FLenv$y)

test_that("Wald-Wolfowitz two Sample Test:",{
    result = eval_expect_equal({
        p.val <- res1$p.value
        z <- res1$statistic
    },Renv,FLenv,
    noexpectation=c("res1"),
    expectation =c( "p.val", "z"),
    verbose=T,
    check.attributes=FALSE)
})




#Wald-Wolf Test 2s

Renv = new.env(parent = globalenv())
Renv$a = c(35,44,39,50,48,29,60,75,49,66)
Renv$b = c(17, 23, 13, 24, 33, 21, 18, 16, 32)
FLenv = as.FL(Renv)
FLenv$res1 <- WaldWolftest2s(FLenv$a, FLenv$b)
Renv$res1 <- runs.test(Renv$a, Renv$b, alternative = "two.sided")



                                        # source: http://www.statext.com/practice/WaldWolfowitzRunsTestTwo02.php
                                        #result of source gives Z = -2.83 which is same as computed by FL Test

test_that("Wald-Wolfowitz two Sample Test:",{
    result = eval_expect_equal({
        p.val <- res1$p.value
        z <- res1$statistic
    },Renv,FLenv,
    noexpectation=c("res1"),
    expectation =c( "p.val", "z"),
    verbose=T,
    check.attributes=FALSE)
})
