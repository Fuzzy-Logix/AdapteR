library(rpart)
Renv=new.env(parent= globalenv())
FLenv= as.FL(Renv)
Renv$kyphosis<-kyphosis
colnames(Renv$kyphosis)<-paste0("Col",1:ncol(kyphosis))
FLenv$kyphosis<-as.FLTable(Renv$kyphosis,temporary=FALSE)

FLenv$deeptable<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$deeptable<-as.data.frame(FLenv$deeptable)

test_that("FLrpart: test for deep tables",{

	robj<-rpart(Renv$deeptable,formula= Renv$deeptable$`-1`~.,method="class")
	flobj<-rpart(FLenv$deeptable,formula= -1~.)
	FLexpect_equal(robj$frame$var,flobj$frame$var)
	FLexpect_equal(robj$frame$n,flobj$frame$n)
})

test_that("FLrpart: test for wide tables",{
	robj<-rpart(Renv$kyphosis,formula= Renv$kyphosis$`-1`~.,method="class")
	flobj<-rpart(FLenv$kyphosis,formula= -1~.)
	FLexpect_equal(robj$frame$var,flobj$frame$var)
	FLexpect_equal(robj$frame$n,flobj$frame$n)
})
