## Since both the objects need to be dealt differently, objects have been initialized and 
## defined outside test_that function


## Since FL and R environments have different case structure for column names


## step function does not work properly. throws an error
## asana ticket- https://app.asana.com/0/143316600934101/158092657328842

options(debugSQL=T)
test_that("test for step", {
    flData <- FLTable("FL_DEMO.tblAbaloneWide", "ObsID")
    rData<-as.data.frame(flData)
    colnames(rData)<-colnames(flData)
    scope <- list(lower=Diameter~Height+ShellWeight,upper=Diameter~Height+ShellWeight+ShuckedWeight)
    r <- step(lm(scope$upper, data = rData), scope = scope, direction = "backward")
    fl <- step(flData, scope = scope, direction = "backward")
    
    expect_equal(coef(r),coef(fl))

    fl <- step(flData, scope = scope, direction = "forward")
    
    expect_equal(coef(r),coef(fl))
})
