Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)

Renv$response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
Renv$fact <- gl(3,1,30,labels=LETTERS[1:3])
Renv$block <- gl(10,3,labels=letters[1:10])
Renv$data <- data.frame(response=Renv$response,
                        block=Renv$block,
                        fact=Renv$fact)

## Explicitly assigning in FLenv so that same table is re-used
FLenv$data <- as.FLTable(Renv$data,tableName = getOption("TestTempTableName"),temporary=F, drop = TRUE)

## Results dont match for p.value and FL does not have p.value.multcomp
test_that("cochran-Q Test on FLTable: R example ",{
    result = eval_expect_equal({
        fit <- cochran.qtest(response~fact|block,data=data)
        pValue <- fit$p.value
    },Renv,FLenv,
    expectation=c("estimate"),
    noexpectation=c("fit"))
})

