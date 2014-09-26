#  Fuzzy Logix, LLC: Functional Testing Script for R-Wrappers for DB Lytix functions on Teradata
# 
#  Copyright (c): 2014 Fuzzy Logix, LLC
# 
#  NOTICE: All information contained herein is, and remains the property of Fuzzy Logix, LLC.
#  The intellectual and technical concepts contained herein are proprietary to Fuzzy Logix, LLC.
#  and may be covered by U.S. and Foreign Patents, patents in process, and are protected by trade
#  secret or copyright law. Dissemination of this information or reproduction of this material is
#  strictly forbidden unless prior written permission is obtained from Fuzzy Logix, LLC.
# 
# 
#  Functional Test Specifications:
# 
#       Test Category:          Data Mining Functions##
#       Test Unit Number:       FLKMeans-TD-03
# 
#       Name(s):                FLKMeans
# 
#       Description:            K-Means clusters the training data. The relationship of
#                               observations to clusters has hard edges.
#       Applications:
# 
#       Signature:              FLKMeans (IN TableName FLTable, IN centers Numeric,
#                               IN iter.max Numeric, IN nstart Numeric, IN Note Character, IN PrimaryKey Character, IN Exclude Character Vector, IN ClassSpec List, IN WhereClause Character))
# 
#       Parameters:             See Documentation
# 
#       Return value:           Object of class FLKMeans
# 
#       Last Updated:           09-19-2014
# 
#       Author:                 <mitul.mundra@fuzzyl.com>

-- BEGIN: TEST SCRIPT
# cleanup workspace

rm(list=ls())
setwd("D:/R wrappers/RWrappers/trunk")

# include
source("S4//S4FLTable.r")
source("S4//FLDataPrep.r")
source("S4//FLkmeans.r")
source("S4//utilities.r")
source("S4//results.r")

library(RODBC)
DBConnect <- odbcConnect("Gandalf")

# Create FLTable object
Tbl <-  FLTable(DBConnect, DBName = "FL_TRAIN", TableName = "fzzlKMeansDemo")



#  BEGIN: POSITIVE TEST(s)

#  Test with normal and extreme values

#  Case 1a:
#  Perform KMeans with non-sparse data
res <- FLKMeans(Tbl, 2, iter.max = 20, nstart = 2, PrimaryKey = "ID", WhereClause = NULL)
#  Result: standard outputs

#  Display result
res <- fetch.results(res)
res 

#  Clear result
rm(res)

#  Case 1b:
#  Perform KMeans with non-sparse data
res <- FLKMeans(Tbl, 2, iter.max = 20, nstart = 2, PrimaryKey = "ID", WhereClause = "Num_Val <> 0")
#  Result: standard outputs

#  Display result
res <- fetch.results(res)
res 

#  Clear result
rm(res)

