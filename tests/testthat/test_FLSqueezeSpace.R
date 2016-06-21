

#Not in R .
#For ODBC Connection with FL_DEMO database.

test_that("Check for FLCleanStr function",{
         widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
         flv <- widetable[1:6,"string"]
         print(flv)
         resultflvector <- FLSqueezeSpace(flv)
         print(resultflvector)
       })

#Not in R .
#For JDBC Connection with fuzzylogix database.
test_that("Check for FLCleanStr function",{
         widetable  <- FLTable("fuzzylogix", "tblAutoMpg", "ObsID")
         flv <- widetable[1:6,"CarName"]
         print(flv)
         resultflvector <- FLSqueezeSpace(flv)
         print(resultflvector)
       })