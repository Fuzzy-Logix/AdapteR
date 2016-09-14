Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)

require(survival)

## data1 is DBLytix example
## data2 is R example

Renv$data1 <- sqlQuery(connection,paste0("SELECT DataSetID,Gender,TIME_VAL,STATUS ",
                                        " FROM vwWHAS100 ORDER by 1,2"))
Renv$fit1 <- dlply(Renv$data1,c("DataSetID"),
              function(x)
                survival::survdiff(Surv(TIME_VAL,STATUS)~Gender,
                                    data=x))

## Some pre-processing needed to add DatasetID or ObsID
Renv$data2 <- cbind(datasetID=1,ovarian)
Renv$fit2 <- survival::survdiff(Surv(futime, fustat) ~ rx,
                                data=Renv$data2)


FLenv$data1 <- FLTable("vwWHAS100","DataSetID")
FLenv$fit1 <- survdiff(Surv(TIME_VAL,STATUS)~Gender,
                        data=FLenv$data1)
FLenv$data2 <- as.FLTable(Renv$data2,uniqueIdColumn=1)
FLenv$fit2 <- survdiff(Surv(futime, fustat) ~ rx,
                        data=FLenv$data2)

test_that("2 sample Kaplan-Meier Multi Datasets: Without var",{
    result = eval_expect_equal({
            res1 <- lapply(fit1,function(x){
                      x$var <- NULL
                      x$p.value <- NULL
                      x$chisqApprox <- NULL
                      x$call <- NULL
                })
            res2 <- lapply(fit2,function(x){
                      x$var <- NULL
                      x$p.value <- NULL
                      x$chisqApprox <- NULL
                      x$call <- NULL
                })
    },Renv,FLenv,
    check.attributes=FALSE)
})
