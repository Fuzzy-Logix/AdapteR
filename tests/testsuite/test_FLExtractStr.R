test_that("FLExtractStr",{
  widetable  <- FLTable(getTestTableName("tblAutoMpg"),"ObsID")
  flv <- widetable[1:6,"carName"]
  resultflvector <- FLExtractStr(flv,"A",1)
})
