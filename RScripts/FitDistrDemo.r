rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include 
source("S4//FLFitDistribution.r")
source("S4//utilities.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")

########################################################################################################

# Demo for FLFitDistr
# Create FLFitDistrObject
# Normal Distribution
Data <-  FLFitDistrObject(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrapperFitNormalDistrTest", DistributionType = "Cont")
# Fit Distribution
FitDistrRes <- FLFitDistr(Data, "Normal", "MDE")

# Weibull Distribution
Data <-  FLFitDistrObject(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrapperFitWeibullDistrTest", DistributionType = "Cont")
# Fit Distribution
FitDistrRes <- FLFitDistr(Data, "Weibull", "MDE")

# Binomial Distribution
Data <- FLFitDistrObject(DBConnect, DBName = "FL_R_WRAP", TableName = "RWrapperFitBinomialDistrTest", DistributionType = "Disc")
# Fit Distribution
FitDistrRes <- FLFitDistr(Data, "Binomial", "MDE")



########################################################################################################