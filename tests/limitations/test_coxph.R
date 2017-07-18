Renv = new.env(parent = globalenv())
#fit <- coxph(Surv(time, status) ~ age + sex, lung1) 
FLenv = as.FL(Renv)

Renv$lung1 <- survival::lung
names(Renv$lung1) <- c("inst", "thetime", "status", "age", "sex", "ph.ecog", "ph.karno", 
"pat.karno", "meal.cal", "wt.loss")
Renv$lung1$status <- Renv$lung1$status - 1
#FLenv <- new.env(parent = globalenv())
FLenv$lung1 <- as.FLTable(Renv$lung1, tableName= getOption("TestTempTableName"),
                         temporary= FALSE, drop= TRUE)

# options(debugSQL = FALSE)

test_that("cox coefficients  https://app.asana.com/0/136555696724838/163682320948854 ",{
  result = eval_expect_equal({
      coxobj <- coxph(Surv(thetime, status) ~ age + sex, lung1)
      coeffcox <- coxobj$coefficients
  },Renv,FLenv,
  expectation = "coeffcox",
  noexpectation = c("coxobj","lung1"),
  check.attributes=F,
  tolerance = .000001,
  verbose = T
  )
}) 



test_that("cox: equality of  nevent, n, waldscore, means attributes https://app.asana.com/0/136555696724838/163682320948854 ",{
    result = eval_expect_equal({
        nevent <- coxobj$nevent
        n <- coxobj$n
        wald.test <- coxobj$wald.test
        means <- coxobj$means
    },Renv,FLenv,
    expectation=c("nevent", "n", "wald.test", "means"),
    noexpectation = "coxobj",
    tolerance = .000001,
    verbose = T
    )
})


test_that("cox: equality of loglik https://app.asana.com/0/136555696724838/163682320948854 ",{
    FLloglik <- c(FLenv$coxobj$loglik[1,1], FLenv$coxobj$loglik[1,2])
    Rloglik <- Renv$coxobj$loglik
    result= expect_equal(FLloglik, Rloglik, tolerance= 0.0001)
})



test_that("cox linear.predictors  https://app.asana.com/0/136555696724838/163682320948854 ",{
  result = eval_expect_equal({
    coefflin <- coxobj$linear.predictors
  },Renv,FLenv,
  expectation = "coefflin",
  noexpectation = c("coxobj","lung1"),
  check.attributes=F,
  tolerance = .000001,
  verbose = T
  )
})

#summary, plot??

## DBLytix manual example
fldata <- FLTable(getTestTableName("tblcoxph_wide"),"ObsID")
rdata <- as.R(fldata)
fldata@Dimnames[[2]] <- tolower(fldata@Dimnames[[2]])
colnames(rdata) <- tolower(colnames(rdata))
vformula <- Surv(time_val,status) ~ sex + ivdrug + tx
rcoxobj <- coxph(vformula, rdata)
flcoxobj <- coxph(vformula,fldata)
test_that("FLCoxPH: equality of coefficients https://app.asana.com/0/136555696724838/163682320948854 ",{
    expect_equal(coef(rcoxobj),coef(flcoxobj))
})




