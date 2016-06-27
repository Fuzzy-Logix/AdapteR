test_that("FLExtractStr",{
  widetable  <- FLTable("fuzzylogix", "tblAutoMpg", "ObsID")
  flv <- widetable[1:6,"carName"]
  resultflvector <- FLExtractStr(flv,"A",1)
})
