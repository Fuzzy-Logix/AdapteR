#cleanup
rm(list=ls())
setwd("C:/Users/STPL/Fuzzy Logix/RWrappers/trunk")
#includes
library(RODBC)
source("S4//S4FLTable.r")
source("S4//utilities.r")
source("S4//FLAnova2Way.r")

library(HypoR)
DBConnect <- odbcConnect("Gandalf")
Tbl2 <-  FLTable(DBConnect, "FL_R_WRAP", "tblAutoMpg")
FLAncova(Tbl2,"MPG","Origin","Weight")

library(HypoR)
DBConnect <- odbcConnect("Gandalf")
Tbl2 <-  FLTable(DBConnect, "FL_R_WRAP", "tblAutoMpg")
FLAnova2Way(Tbl2,"MPG","Origin","CarName")


Tbl <-  FLTable(DBConnect, "FL_R_WRAP", "tblAnova2WaySingle")
FLAnova2Way(Tbl,"num_val","gender","Age")

# ANCOVA 

Tbl2 <-  FLTable(DBConnect, "FL_R_WRAP", "tblAncovaTest")
FLAncova(Tbl2,"YVAL","GROUPID","XVAL")

Tbl2 <-  FLTable(DBConnect, "FL_R_WRAP", "tblAutoMpg")
FLAncova(Tbl2,"MPG","Origin","Weight")
