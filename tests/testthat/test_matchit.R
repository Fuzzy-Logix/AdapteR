## https://fuzzyl.atlassian.net/browse/FAI-168
dataf<- data.frame(var1 = rnorm(200),
                     var2 = rnorm(200), 
                     var3 = sample( c(0, 1), 200, replace = TRUE))
flt<- as.FLTable(dataf,tableName = getOption("TestTempTableName"),temporary=F, drop = TRUE)

test_that("matchit:: execution works ",{
	vres <- matchit(var3~.,flt)
	vres$discared
	vres$propensities
	vres$treat
})
