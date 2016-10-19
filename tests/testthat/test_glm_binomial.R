Renv = new.env(parent = globalenv())
Renv$dataf<- data.frame(var1 = rnorm(200),
                        var2 = rnorm(200), 
                        var3 = sample( c(0, 1), 200, replace = TRUE))
FLenv <- as.FL(Renv)


test_that("glm: execution for binomial ",{
  result = eval_expect_equal({
    glmobj <- glm(var3 ~ var1 + var2, data=dataf, family = "binomial")
    coeffs <- coef(glmobj)
  },Renv,FLenv,
  expectation = "coeffs",
  noexpectation = "glmobj",
  check.attributes=F,
  tolerance = .000001
  )
}) 



test_that("glm: equality of coefficients, residuals, fitted.values, df.residual for binomial",{
    result = eval_expect_equal({
        coeffs2 <- glmobj$coefficients
        res <- as.vector(glmobj$residuals)
        fitteds <- as.vector(glmobj$fitted.values)
        names(res) <- names(fitted) <- NULL ## todo: support names in AdapteR
        dfres <- glmobj$df.residual
    },Renv,FLenv,
    expectation=c("coeffs2","res",
                "fitteds","dfres"),
    noexpectation = "glmobj",
    tolerance = .000001,
    check.attributes = F
  )
})

