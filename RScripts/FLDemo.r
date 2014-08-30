rm(list=ls())
source("S3FLTable.r")
source("S3FLkmeans.r")

#Prep Demo

Tbl <- FLTable(DSN="Gandalf",DBName="FL_TRAIN",TableName="tblAbaloneWideTest")
TblPrep <- FLDataPrep(Tbl,Exclude=c("Rings"),ClassSpec=list(SEX="M",DummyCat="D"))

#KMeans Demo

Tbl2 <- FLTable(DSN="Gandalf",DBName="FL_TRAIN",TableName="tblUSArrests")
Tbl2[["DeepTableName"]] = "tblUSArrests"
res <- FLkmeans(Tbl2, 2, iter.max = 20, nstart = 2)