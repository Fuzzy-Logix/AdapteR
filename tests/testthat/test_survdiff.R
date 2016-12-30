Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)

## data1 is DBLytix example
## data2 is R example

Renv$data1 <- sqlQuery(connection,paste0("SELECT DataSetID,Gender,TIME_VAL,STATUS ",
                                        " FROM vwWHAS100 ORDER by 1,2"))
Renv$fit1 <- dlply(Renv$data1,c("DataSetID"),
              function(x)
                survival::survdiff(Surv(TIME_VAL,STATUS)~Gender,
                                    data=x))
Renv$data2 <- survival::ovarian
Renv$fit2 <- survival::survdiff(Surv(futime, fustat) ~ rx,
                                data=Renv$data2)


FLenv$data1 <- FLTableMD("vwWHAS100","DataSetID","ObsID")
FLenv$fit1 <- survdiff(Surv(TIME_VAL,STATUS)~Gender,
                        data=FLenv$data1)
# dropFLTestTable()
FLenv$data2 <- as.FLTable(Renv$data2,
                        tableName="ARBaseTestTempTable",
                        drop=TRUE)
FLenv$fit2 <- survdiff(Surv(futime, fustat) ~ rx,
                        data=FLenv$data2)

test_that("2 sample Kaplan-Meier Multi Datasets: Without var",{
    result = eval_expect_equal({
            res1 <- lapply(fit1,function(x){
                      x$var <- NULL
                      x$p.value <- NULL
                      x$chisqApprox <- NULL
                      x$call <- NULL
                      x
                })
            res2 <- lapply(list(fit2),function(x){
                      x$var <- NULL
                      x$p.value <- NULL
                      x$chisqApprox <- NULL
                      x$call <- NULL
                      x
                })[[1]]
    },Renv,FLenv,
    expectation=c("res1","res2"),
    check.attributes=FALSE)
})
