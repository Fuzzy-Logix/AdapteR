Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)

require(RVAideMemoire)

## data1 is DBLytix example
## data2 is R example
Renv$response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
Renv$fact <- gl(3,1,30,labels=LETTERS[1:3])
Renv$block <- gl(10,3,labels=letters[1:10])
Renv$data <- data.frame(response=Renv$response,
                        block=Renv$block,
                        fact=Renv$fact)

## Explicitly assigning in FLenv so that same table is re-used
dropFLTestTable()
FLenv$data <- as.FLTable(Renv$data,
    tableName="ARBaseTestTempTable")

## Results dont match for p.value
## FL does not have p.value.multcomp
test_that("cochran-Q Test on FLTable: R example: checking Result Equality without p.value:",{
    result = eval_expect_equal({
            fit <- cochran.qtest(response~fact|block,data=data)
            fit$p.value <- NULL
            fit$p.value.multcomp <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})
