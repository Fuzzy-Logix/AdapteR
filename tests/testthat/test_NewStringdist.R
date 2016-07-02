Renv<-new.env(parent = globalenv())
Renv$a<-c('foo','bar','boo','baz')
Renv$b<-c("foo","bar","boo")

FLenv <- as.FL(Renv)

test_that("testing R and FL Results for String Functions for different methods ",{
  result1=eval_expect_equal({
    resultflvector1 <- stringdist("xyz",a,method="dl")
    resultflvector2 <- stringdist("xyz",a,method="lv",caseFlag=1)
    resultflvector3 <- stringdist(a,"xyz",method="hamming")
    resultflvector5 <- stringdist(c("xyz","juio"),a,method="jaccard")
    resultflvector6 <- stringdist(c("xyz","juio"),a,method="jw")
  },Renv,FLenv)
})

test_that("stringdistmatrix R and FL results match ",{
  result2=eval_expect_equal({
    test1<-stringdistmatrix(a)
    test3<-stringdist(a,b)
    test4<-stringdistmatrix(a,b)
    test5<-stringdistmatrix("baro",b)
  },Renv,FLenv,check.attributes=FALSE)
})
