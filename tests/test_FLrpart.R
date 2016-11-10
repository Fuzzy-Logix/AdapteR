## tests are failing for now. Will need more subsetting and precision on
## what parameters are to be tested.

library(rpart)
Renv=new.env(parent= globalenv())
FLenv= as.FL(Renv)
Renv$kyphosis<-kyphosis
colnames(Renv$kyphosis)<-paste0("Col",1:ncol(kyphosis))
FLenv$kyphosis<-as.FLTable(kyphosis,temporary=FALSE)

FLenv$deeptable<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$deeptable<-as.data.frame(deeptable)

## fails because objects have different data types
test_that("FLrpart: test for deep tables",{
	robj<-rpart(Renv$deeptable,formula= Renv$deeptable$`-1`~.,method="class")
	flobj<-FLrpart(FLenv$deeptable,formula= -1~.)
	FLexpect_equal(robj,flobj)
})

## fails because Regression data prep acts weirdly when dependent column is a categorical variable.
test_that("FLrpart: test for wide tables",{
	robj<-rpart(Renv$kyphosis,formula= Col1~.)
	flobj<-FLrpart(FLenv$kyphosis, formula=Col1~.)
	FLexpect_equal(robj,flobj)
})
