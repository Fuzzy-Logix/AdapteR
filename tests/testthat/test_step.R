Renv = new.env(parent = globalenv())

FLenv = as.FL(Renv)

## Since both the objects need to be dealt differently, objects have been initialized and 
## defined outside test_that function
if(!is.TDAster()){
    Renv$vobsidCol <- "ObsID"
    Renv$vscope <- list(lower=Diameter~Height,
                upper=Diameter~Height+ShellWeight+ShuckedWeight)
    Renv$scope <- list(lower=Diameter~Height+ShellWeight,
                upper=Diameter~Height+ShellWeight+ShuckedWeight)
}else{
    Renv$vobsidCol <- "obsid"
    Renv$vscope <- list(lower=diameter~height,
                upper=diameter~height+shellweight+shuckedweight)
    Renv$scope <- list(lower=diameter~height+shellweight,
                upper=diameter~height+shellweight+shuckedweight)
}

FLenv$vobsidCol <- Renv$vobsidCol
FLenv$vscope <- Renv$vscope
FLenv$scope <- Renv$scope

FLenv$widetable <- FLTable(getTestTableName("tblAbaloneWide"), FLenv$vobsidCol)
Renv$widetable<-as.data.frame(FLenv$widetable)

## Since FL and R environments have different case structure for column names
colnames(Renv$widetable)<-colnames(FLenv$widetable)

## step function does not work properly. throws an error
## asana ticket- https://app.asana.com/0/143316600934101/158092657328842

##options(debugSQL = T)

test_that("test for step", {
  eval_expect_equal({
    r<-step(widetable,scope = vscope, 
                    direction = "backward")
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
    flData <- FLTable(getTestTableName("tblAbaloneWide"), Renv$vobsidCol)
    rData<-as.data.frame(flData)
    colnames(rData)<-colnames(flData)
    
    r <- step(lm(Renv$scope$upper, data = rData), scope = Renv$scope, direction = "backward")
    fl <- step(flData, scope = Renv$scope, direction = "backward")
    expect_equal(coef(r),coef(fl))
    fl <- step(flData, scope = Renv$scope, direction = "forward")
    expect_equal(coef(r),coef(fl))
    test_that("step: access results with $: https://app.asana.com/0/143316600934101/158092657328842",{
        expect_equal(r$coefficients,fl$coefficients)
        expect_equal(r[["coefficients"]],
                    fl[["coefficients"]])
    })
})
