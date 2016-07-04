test_that("FLParseXML",{
  wtd <- FLTable(getOption("ResultDatabaseFL"),"tblXMLTest","GroupID")
  flv <- wtd[,"pXML"]
  resultdataframe <- FLParseXML(flv)
  ## gk: please check if results actually make sense!
})
