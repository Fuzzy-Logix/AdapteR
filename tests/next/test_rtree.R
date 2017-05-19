## The function works for FLRev_4878 database, not for FL_Demo or FL_Train

connection <- flConnect(odbcSource = "Gandalf",database = "FLRev_4878",platform="TD",pkg = "dbc")
flt<-FLTable("tblRegrTree","ObsID","VarID","Num_Val",whereconditions = c("GroupID=1","ObsID<500"))

rtbl<-as.R(flt)
colnames(rtbl)<-paste0("Col",colnames(rtbl))
test_that("test for rtree on deeptables",{
  flobj<-rtree(flt, formula = -1~.,ntree=10)
  flpred<-predict(flobj)
  robj<-randomForest(rtbl, formula = rtbl$`Col-1`~.,ntree=10)
  rpred<-predict(robj)
  result1= expect_equal(robj$ntree,length(flobj$forest))
  result2= expect_equal(as.numeric(rownames(flobj$deeptable)),as.numeric(1:length(robj$y)))
  result3= expect_equal(nrow(flpred),length(rpred))
})

