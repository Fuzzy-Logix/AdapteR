test_that("FLExtractStr",{
  widetable  <- FLTable("tblAutoMpg","ObsID")
  flv <- widetable[1:6,"carName"]
  resultflvector <- FLExtractStr(flv,"A",1)
})
