rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include
source("S4//FLMatrix.r")
source("S4//utilities.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")

########################################################################################################

# Demo for FLCholeskyDecomp
source("S4//FLCholeskyDecomp.r")
# Create FLMatrix object
InMatrix <-  FLMatrix(DBConnect, DBName = "FL_R_WRAP", MatrixTableName = "tblMatrixMulti", MatrixID = 5)
# Perform Cholesky Decomposition
OutMatrix <- FLCholeskyDecomp(InMatrix)

########################################################################################################