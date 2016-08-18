Renv = new.env(parent = globalenv())

var1 <- rnorm(200)
var2 <- rnorm(200)
var3 <- sample(c(0, 1), 200, replace = TRUE)
dataf<- data.frame(var1 = var1,var2 =var2, var3 = var3,offset=1)
#rownames(var4) <- 1:nrow(var4)
Renv$dataf <- dataf
FLenv = as.FL(Renv)


test_that("glm: execution for poisson ",{
  result = eval_expect_equal({
    glmobj <- glm(var3 ~ var1 + var2, data=dataf, family = "poisson")
    coeffs <- coef(glmobj)
  },Renv,FLenv,
  expectation = "coeffs",
  noexpectation = "glmobj",
  check.attributes=F,
  tolerance = .000001)
}) 




test_that("glm: equality of coefficients, residuals, fitted.values, df.residual for poisson",{
    result = eval_expect_equal({
        coeffs2 <- glmobj$coefficients
        res <- glmobj$residuals
        fitteds <- glmobj$fitted.values
        dfres <- glmobj$df.residual
    },Renv,FLenv,
    noexpectation = "glmobj",
    tolerance = .000001,
    check.attribute = F
  )
})

#summary, plot??
