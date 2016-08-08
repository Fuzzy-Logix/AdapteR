connection <- flConnect(odbcSource = "TDVM",database = "fuzzylogix",platform="TD")
devtools::load_all(".")
Renv = new.env(parent = globalenv())

FLenv = as.FL(Renv)

## Since both the objects need to be dealt differently, objects have been initialized and 
## defined outside test_that function

FLenv$widetable <- FLTable("fuzzylogix", "tblAbaloneWide", "ObsID")
Renv$widetable<-as.data.frame(FLenv$widetable)

## Since FL and R environments have different case structure for column names
colnames(Renv$widetable)<-colnames(FLenv$widetable)

formula<-Diameter~ShellWeight+Height+Rings+WholeWeight



FLenv$stepobj<-FLenv$widetable
Renv$stepobj<-lm(formula, data = Renv$widetable)

test_that("test for step function", {
  result=eval_expect_equal({
     r<-step(stepobj,scope = list(lower=Diameter~Height+Rings,upper=formula, direction = "backward"))
    c<-sort(coefficients(r))
  },
  Renv,FLenv,
  expectation="c",
  noexpectation=c("obj","r")
  
  )
}
)
parts <- c("coefficients","residuals","df.residual",
           "rank","terms")


test_that("lm: equality of coefficients, residuals, rank and terms",{
  result = eval_expect_equal({
    sapply(parts,
           function(i){
             assign(i,do.call("$",list(r,i)))
           })
    modelDim <- dim(r$model)
  },Renv,FLenv,
  noexpectation = "r",
  expectation = c(parts,"modelDim"),
  check.attributes=F)
}
)
