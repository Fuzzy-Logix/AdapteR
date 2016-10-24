##testing string distance functions
Renv<-new.env(parent = globalenv())
Renv$a<-c('foo','bar','boo','baz')
Renv$b<-c("foo","bar","boo")
Renv$c<-c("baz","buz")
Renv$d<-c('a','b','c')
Renv$e<-c('a','c')
Renv$ab <- 'ab'
Renv$ba <- 'ba'
Renv$ca <- "ca"
Renv$abc <- 'abc'
Renv$ABC <- 'ABC'
Renv$cba <- 'cba'
Renv$hello <- "hello"
Renv$HeLl0 <- "HeLl0"
Renv$MARTHA <- 'MARTHA'
Renv$Euler <- 'Euler'
Renv$Ellery <- 'Ellery'
Renv$MATHRA <- 'MATHRA'
Renv$weight<-c(1,1,1,0.5)
Renv$weighta<-c(0.5,1,1,1)
FLenv <- as.FL(Renv)

## # Simple example using optimal string alignment
## stringdist(ca,abc)
test_that("stringdist: string, string, Damerau-Levenshtein",{
    result1=eval_expect_equal({
        test1<-stringdist(ca,abc,method="dl")
    },Renv,FLenv,
    expectation=c("test1"))
})

## # computing a 'dist' object
test_that("stringdistmatrix: vector, Damerau-Levenshtein",{
    result2=eval_expect_equal({
        test2<-stringdistmatrix(a,method="dl")
        ##print(test2)
        ##plot(hclust(test2))
    },Renv,FLenv,
    expectation=c("test1"),
    check.attributes=FALSE)
})

## # The following gives a matrix
## stringdistmatrix(c("foo","bar","boo"),c("baz","buz"))
test_that("stringdistmatrix: vector, vector, Damerau-Levenshtein",{
    result3=eval_expect_equal({
        test3<-stringdistmatrix(b,c,method="dl")
    },Renv,FLenv,check.attributes=FALSE)
})

## # string distance matching is case sensitive:
## stringdist(ABC,abc)
test_that("stringdist: case sensitive, Damerau-Levenshtein",{
    result4=eval_expect_equal({
        test4<-stringdist(ABC,abc, method="dl",caseFlag = 1)
        testtolower<-stringdist(tolower(ABC),abc,method="dl")
    },Renv,FLenv)
})

## stringdist(c('a','b','c'),c('a','c'))
test_that("stringdist:  recycles the shortest argument",{
    result6=eval_expect_equal({
        test6<-stringdist(d,e,method="dl")
    },Renv,FLenv)
})


## # stringdistmatrix gives the distance matrix (by default for optimal string alignment):
## stringdist(c('a','b','c'),c('a','c'))
test_that("stringdist: string distance",{
    result7=eval_expect_equal({test7<-stringdist(d,e,method="dl")},Renv,FLenv)
})


## # Non-unit weights for insertion and deletion makes the distance metric asymetric
## stringdist(ca,abc)
## stringdist(abc,ca)
## stringdist(ca,abc,weight=c(0.5,1,1,1))
## stringdist(abc,ca,weight=c(0.5,1,1,1))
                                        #ERROR: vector not supported in stringdist
test_that("stringdist: string distance",{
    result9=eval_expect_equal({
        test9<-stringdist(ca,abc,method="dl")
        test10<-stringdist(abc,ca,method="dl")
        test11<-stringdist(ca,abc,weighta,method="dl",useBytes=TRUE)
        test12<-stringdist(abc,ca,weighta,method="dl",useBytes=TRUE)
    },Renv,FLenv)
})


test_that("stringdist: Damerau-Levenshtein",{
    result19=eval_expect_equal({
        test19<-stringdist(ca,abc,method="dl")},Renv,FLenv)
})

test_that("stringdist: Levenshtein",{
    result20= eval_expect_equal({
        test20<-stringdist(ca,abc,method="lv")},Renv,FLenv)
})


test_that("stringdist: Jaccard" ,{
    result23=eval_expect_equal({
        test23<-stringdist(MARTHA,MATHRA,method='jaccard')},Renv,FLenv)
})

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
    test1<-stringdistmatrix(a,method="dl")
    test3<-stringdist(a,b,method="dl")
    test4<-stringdistmatrix(a,b,method="dl")
    test5<-stringdistmatrix("baro",b,method="dl")
  },Renv,FLenv,check.attributes=FALSE)
})
