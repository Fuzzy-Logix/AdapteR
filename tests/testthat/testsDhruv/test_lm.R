widetable  <- FLTable(getOption("ResultDatabaseFL"), "tblAbaloneWide", "ObsID")
dataframe1 <- as.data.frame(widetable)

object<-lm(Rings~Height+Diameter,widetable)
expected<-lm(Rings~Height+Diameter,dataframe1)

test_that("check_for_lm_function",{
  
  FLexpect_equal(object$coefficients,sort(expected$coefficients,decreasing = FALSE),check.attributes =FALSE)
  FLexpect_equal(residuals(object),residuals(expected),check.attributes =FALSE)
  FLexpect_equal(as.vector(object$fitted.values),expected$fitted.values,check.attributes = FALSE)
  FLexpect_equal(object$model,expected$model,check.attributes = FALSE)
  FLexpect_equal(predict(object),predict(expected),check.attributes = FALSE)
  FLexpect_equal(influence(object),influence(expected),check.attributes = FALSE)
  FLexpect_equal(object$model,expected$model,check.attributes = FALSE)
  FLexpect_equal(object$rank,expected$rank,check.attributes = FALSE)
  FLexpect_equal(object$assign,expected$assign,check.attributes = FALSE)
  FLexpect_equal(object$df.residuals,expected$df.residuals,check.attributes = FALSE)

    })