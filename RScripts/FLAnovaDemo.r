#cleanup
rm(list=ls())
setwd("C:/Users/STPL/Fuzzy Logix/RWrappers/trunk")
#includes
library(RODBC)
source("S4//S4FLTable.r")
source("S4//utilities.r")
source("S4//FLAnova2Way.r")

DBConnect <- odbcConnect("Gandalf")
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "tblAnova2WaySingle")
FLAnova2Way(Tbl,"num_val","gender","Age")