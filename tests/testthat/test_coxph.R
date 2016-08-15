Renv = new.env(parent = globalenv())
#fit <- coxph(Surv(time, status) ~ age + sex, lung1) 


Renv$lung1 <- survival::lung
names(Renv$lung1) <- c("inst", "thetime", "status", "age", "sex", "ph.ecog", "ph.karno", 
"pat.karno", "meal.cal", "wt.loss")
Renv$lung1$status <- Renv$lung1$status - 1
FLenv = as.FL(Renv)

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
    result = eval_expect_equal({
        loglik <- coxobj$loglik
    },Renv,FLenv,
    expectation=c("loglik"),
    noexpectation = "coxobj",
    tolerance = .000001,
    verbose = T
    )
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
