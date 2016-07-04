

# #Not in R .
# #For ODBC Connection with FL_DEMO database.
# ## Table tblstringID does not exist.
# test_that("Check for FLCleanStr function",{
#          widetable  <- FLTable(getOption("ResultDatabaseFL"), "tblstringID", "stringID")
#          flv <- widetable[1:6,"string"]
#          ##         print(flv)
#          resultflvector <- FLIsNumeric(flv)
#          ##         print(resultflvector)
#        })

#Not in R .
#For JDBC Connection with fuzzylogix database.
test_that("Check for FLCleanStr function",{
         widetable  <- FLTable(getOption("ResultDatabaseFL"), "tblAutoMpg", "ObsID")
         flv <- widetable[1:6,"CarName"]
         ##         print(flv)
         resultflvector <- FLIsNumeric(flv)
         ##         print(resultflvector)
       })
