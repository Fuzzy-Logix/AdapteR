Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)
require(survival)

Renv$data <- sqlQuery(connection,paste0("SELECT DataSetID,Gender,TIME_VAL,STATUS ",
                                        " FROM vwWHAS100 ORDER by 1,2"))
Renv$fit <- dlply(Renv$data,c("DataSetID","Gender"),
              function(x)
                survival::survfit.formula(Surv(TIME_VAL,STATUS)~1,
                                        data=x,
                                        conf.type="plain"))

FLenv$data <- FLTableMD("vwWHAS100","DataSetID","ObsID")
FLenv$fit <- survfit(Surv(TIME_VAL,STATUS)~1,
                    data=FLenv$data,
                    GroupBy="Gender")

## Testing with R example
dat <- aml
colnames(dat) <- c("ftime","fstatus","x")
Renv$data2 <- dat

# dropFLTestTable()
FLenv$data2 <- as.FLTable(Renv$data2,
                        tableName="ARBaseTestTempTable",
                        drop=TRUE)

FLenv$fit2 <- survfit(Surv(ftime, fstatus) ~ 1, 
                    data = FLenv$data2)
Renv$fit2 <- survival::survfit.formula(Surv(ftime, fstatus) ~ 1, 
                    data = Renv$data2,
                    conf.type="plain")

for(i in 1:2){
    ## Fetch Results
    test_that("Kaplan-Meier with groupBy and dlply result equality: Fetching result",{
        result = eval_expect_equal({
                if(class(fit)!="list")
                    fit <- list(fit)
                res1 <- lapply(fit,function(x){
                          x<-fFetchFLSurvfit(x)
                          x$call<-NULL
                          x$std.err <- NULL
                          x$strata <- NULL
                          x$PetoEstimate <- NULL
                          x
                    })
                # if(length(res1)==1)
                #     res1 <- res1[[1]]
        },Renv,FLenv,
        noexpectation="res1")
    })

    ## NAN in R <=> 0 in FL!
    test_that("Kaplan-Meier with groupBy and dlply result equality: upper and lower",{
        vtemp <- lapply(1:length(Renv$res1),
                    function(x){
                        Rupper <- Renv$res1[[x]]$upper
                        FLupper <- FLenv$res1[[x]]$upper
                        Rlower <- Renv$res1[[x]]$lower
                        FLlower <- FLenv$res1[[x]]$lower
                        expect_equal(Rupper[!is.na(Rupper)],FLupper[!is.na(Rupper)])
                        expect_equal(Rlower[!is.na(Rlower)],FLlower[!is.na(Rlower)])
                    })
    })

    test_that("Kaplan-Meier with groupBy and dlply result equality: Without upper and lower",{
        result = eval_expect_equal({
                 # if(class(res1)!="list")
                 #    res1 <- list(res1)
                 res1 <- lapply(res1,function(x){
                          x$upper <- NULL
                          x$lower <- NULL
                          x
                    })
                 if(length(res1)==1)
                    res1 <- res1[[1]]
        },Renv,FLenv,
        expectation="res1")
    })
    Renv$fit <- Renv$fit2
    FLenv$fit <- FLenv$fit2
}
