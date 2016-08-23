test_that("FLExtractStr",{
  widetable  <- FLTable(getRemoteTableName(tableName="tblAutoMpg", temporaryTable=FALSE), "ObsID")
  flv <- widetable[1:6,"carName"]
  resultflvector <- FLExtractStr(flv,"A",1)
})
