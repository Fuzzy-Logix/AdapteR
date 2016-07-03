
#Not in R .
#For ODBC Connection with FL_DEMO database.
#Only with FLVector with charcters.

test_that("Check for FLCleanStr function",{
         widetable  <- FLTable("FL_DEMO", "tblstringID", "stringID")
         flv <- widetable[1:6,"string"]
         print(flv)
         resultflvector <- FLIsHex(flv)
         print(resultflvector)
       })

#Not in R .
#For JDBC Connection with fuzzylogix database.
#Only with FLVector with charcters.
test_that("Check for FLCleanStr function",{
         widetable  <- FLTable("fuzzylogix", "tblAutoMpg", "ObsID")
         flv <- widetable[1:6,"CarName"]
         print(flv)
         resultflvector <- FLIsHex(flv)
         print(resultflvector)
       })