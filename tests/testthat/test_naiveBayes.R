 
## 
test_that("naiveBayes and its methods run: ",{
    fltbl <- FLTable("tblNBData", "ObsID", "VarID", "NUM_VAL")
 	flmod <-naiveBayes(data= fltbl, formula = ~.)
})

test_that("naiveBayes on iris: Result Comparision: ",{
    rtbl <- iris
	rtbl$Species <- sample(x = 2, size = length(rtbl$Species), replace = TRUE)-1
	rtbl$Species <- as.numeric(rtbl$Species)
	colnames(rtbl) <- tolower(colnames(rtbl))
	fliris <- as.FLTable(rtbl,tableName = getOption("TestTempTableName"),temporary=F, drop = TRUE)
	flmod <-naiveBayes(data = fliris, formula = species~., laplace = 1)
	rtbl$species <- as.factor(rtbl$species)
	rmod <-naiveBayes(data = rtbl, formula = species~.,laplace=1)
	rpred <- predict(rmod,rtbl)
	vpred <- predict(flmod)
	cat("\n .... FL Results printing below.... \n ")
 	print(flmod)
 	cat("\n .... R Results printing below.... \n ")
 	print(rmod)
 	expect_equal(nrow(vpred),nrow(fliris))
})
