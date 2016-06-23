library(testthat)

test_that("FLParseXML",{
  wtd <- FLTable("fuzzylogix","tblXMLTest","GroupID")
  flv <- wtd[,"pXML"]
  resultdataframe <- FLParseXML(flv)
})
