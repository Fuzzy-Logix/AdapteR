connection <- flConnect(odbcSource = "Gandalf",database = "FL_TRAIN",platform="TD")

## Since both the objects need to be dealt differently, objects have been initialized and 
## defined outside test_that function

FLenv$stepobj <- FLTable("FL_DEMO", "tblAbaloneWide", "ObsID")
Renv$widetable<-as.data.frame(FLenv$stepobj)

## Since FL and R environments have different case structure for column names
colnames(Renv$widetable)<-colnames(FLenv$stepobj)

Renv$stepobj<-lm(Diameter~Height+ShellWeight, data = Renv$widetable)

## step function does not work properly. throws an error
## asana ticket- https://app.asana.com/0/143316600934101/158092657328842

test_that("test for step", {
  eval_expect_equal({
    
   r<-step(stepobj,scope = list(lower=Diameter~Height,upper=Diameter~Height+ShellWeight+ShuckedWeight), direction = "backward")
    
  },Renv,FLenv,
  )
  
}
)
