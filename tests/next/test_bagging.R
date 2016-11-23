library(adabag)

fltbl<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
rtbl<-as.data.frame(fltbl)
colnames(rtbl)<-paste0("col",1:ncol(rtbl))
rtbl[[1]]<-as.factor(rtbl[[1]])
robj<-bagging(rtbl,formula= col1~.,mfinal=10)
flobj<-bagging(fltbl,formula= -1~.)

test_that("test for bagging on deeptables",{
	FLexpect_equal(robj,flobj)
})