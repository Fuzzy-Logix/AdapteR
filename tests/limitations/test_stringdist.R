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

## # The lcs (longest common substring) distance returns the number of 
## # characters that are not part of the lcs.
## #
## # Here, the lcs is either 'a' or 'b' and one character cannot be paired:
## stringdist(ab,ba,method="lcs")
## # Here the lcs is 'surey' and 'v', 'g' and one 'r' of 'surgery' are not paired
## stringdist('survey','surgery',method="lcs")
test_that("stringdist: longest common substring distance", {
    result13=eval_expect_equal({
        test13<-stringdist(ab,ba,method="lcs")
        test14<-stringdist('survey','surgery',method="lcs")
    },Renv,FLenv)
})

## # q-grams are based on the difference between occurrences of q consecutive characters
## # in string a and string b.
## # Since each character abc occurs in abc and cba, the q=1 distance equals 0:
## stringdist(abc,cba,method='qgram',q=1)
test_that("stringdist: lcs", {
    result15=eval_expect_equal({
        test15<-stringdist(ab,ba,method="lcs")
    },Renv,FLenv)
})

## # since the first string consists of ab,'bc' and the second 
## # of 'cb' and ba, the q=2 distance equals 4 (they have no q=2 grams in common):
## stringdist(abc,cba,method='qgram',q=2)
test_that("stringdist: qgram", {
    result16=eval_expect_equal({
        test16<-stringdist(ab,ba,method="lcs")
        test17<-stringdist(abc,cba,method='qgram',q=1)
    },Renv,FLenv)
})

## # This gives distance 1 since Euler and Gauss translate to different soundex codes.
## stringdist(Euler,'Gauss',method='soundex')
## # Euler and Ellery translate to the same code and have distance 0
## stringdist(Euler,Ellery,method='soundex')
test_that("stringdist: soundex", {
    result17=eval_expect_equal({
        test17<-stringdist(Euler,'Gauss',method='soundex')
        test18<-stringdist(Euler,Ellery,method='soundex')
    },Renv,FLenv)
})

## Hamming distance is undefined for DB Lytix
## strings of unequal lengths so stringdist returns Inf
## For strings of eqal length it counts the number of unequal characters as they occur
## in the strings from beginning to end
test_that("stringdist: hamming", {
    result26=eval_expect_equal({
        test26<-stringdist(ab,abc,method="h")
        test27<-stringdist(hello,HeLl0,method="h")
    },Renv,FLenv)
})



