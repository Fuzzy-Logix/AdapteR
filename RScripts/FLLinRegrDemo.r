#cleanup
rm(list=ls())
setwd("C:/Users/STPL/Fuzzy Logix/RWrappers/trunk")

#includes
source("S4//S4FLTable.r")
source("S4//results.r")
source("S4//utilities.r")
source("S4//FLDataPrep.r")
source("S4//FLLinRegr.r")
library(RODBC)

# Connect to ODBC
DBConnect <- odbcConnect("Gandalf")

# Attach Table 
Tbl <-  FLTable(DBConnect, DBName = "FL_R_WRAP", TableName = "tblAutoMpg")

# Run Linear Regression
Analysis <- FLLinRegr( Tbl, "MPG", Note = "RWrapper Roadmap Relay Demo", PrimaryKey = "ObsID", Exclude = c("CarNum","CarNumber"), ClassSpec = list(CarName = "BMW") )
Analysis <- fetch.results(Analysis)
Analysis@coeffs
