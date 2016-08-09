

#Not in R .
#For JDBC Connection with fuzzylogix database.
#Only with FLVector with charcters.
test_that("Check for FLCleanStr function",{
         widetable  <- FLTable(getRemoteTableName(tableName="tblAutoMpg", temporaryTable=FALSE), "ObsID")
         flv <- widetable[1:6,"CarName"]
         ##         print(flv)
         resultflvector <- FLIsHex(flv)
         ##         print(resultflvector)
       })
