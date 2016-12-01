connection <- flConnect(odbcSource = "Gandalf",database = "FL_DEMO",platform="TD")
devtools::load_all(".")
Renv = new.env(parent = globalenv())

FLenv = as.FL(Renv)

## Since both the objects need to be dealt differently, objects have been initialized and 
## defined outside test_that function

FLenv$widetable <- FLTable("tblAbaloneWide", "ObsID")
Renv$widetable<-as.data.frame(FLenv$widetable)

## Since FL and R environments have different case structure for column names
colnames(Renv$widetable)<-colnames(FLenv$widetable)

## step function does not work properly. throws an error
## asana ticket- https://app.asana.com/0/143316600934101/158092657328842

options(debugSQL = T)

test_that("test for step", {
  eval_expect_equal({
    
    r<-step(widetable,scope = list(lower=Diameter~Height,upper=Diameter~Height+ShellWeight+ShuckedWeight), direction = "backward")
    
  },Renv,FLenv)
}
)


# sqlQuery(connection,"CALL  FLRegrDataPrep('FL_DEMO.tblAbaloneWide', 
#  'ObsID', 
#          'Diameter', 
#          'FL_DEMO.ARBasetblAbaloneWideD1469186546', 
#          'obs_id_colname', 
#          'var_id_colname', 
#          'cell_val_colname', 
#          0, 
#          0, 
#          0, 
#          0, 
#          0, 
#          1, 
#          0, 
#          'Sex,Num_Length,WholeWeight,VisceraWeight,Rings,DummyCat', 
#          NULL, 
#          NULL, 
#          NULL,AnalysisID)")


## Since both the objects need to be dealt differently, objects have been initialized and 
## defined outside test_that function


## Since FL and R environments have different case structure for column names

## step function does not work properly. throws an error
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
    test_that("step: access results with $: https://app.asana.com/0/143316600934101/158092657328842",{
        expect_equal(r$coefficients,fl$coefficients)
        expect_equal(r[["coefficients"]],
                    fl[["coefficients"]])
    })
})
