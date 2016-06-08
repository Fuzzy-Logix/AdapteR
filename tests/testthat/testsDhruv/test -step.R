widetable  <- FLTable("FL_DEMO", "tblAbaloneWide", "ObsID")
dataframe1 <- as.data.frame(widetable)
 obj <- step(widetable,
          scope=list(lower=Rings~Height+Diameter,
                      upper=Rings~Height+Diameter+Sex+Num_Length),
          direction = "UFbackward")

 exp<-step(dataframe1,
          scope=list(lower=Rings~Height+Diameter,
                      upper=Rings~Height+Diameter+Sex+Num_Length),
          direction = "UFbackward")
    
test_that("check_for_step_function",{
    FLexpect_equal(obj$coefficients,exp$coefficients)
    FLexpect_equal(obj$residuals,exp$residuals)
    FLexpect_equal(obj$rank,exp$rank)
    FLexpect_equal(obj$fitted.values,exp$fitted.values)
    FLexpect_equal(obj$assign,exp$assign)
    FLexpect_equal(obj$qr,exp$qr)
    })

   

