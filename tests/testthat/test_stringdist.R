## gk @richa: please implement tests for the following stringdist::stringdist examples
# ASANA: https://app.asana.com/0/143316600934101/150716904120767

##testing string distance functions
Renv<-new.env(parent = globalenv())
Renv$a<-c('foo','bar','boo','baz')
Renv$b<-c("foo","bar","boo")
Renv$c<-c("baz","buz")
Renv$d<-c('a','b','c')
Renv$e<-c('a','c')
Renv$weight<-c(1,1,1,0.5)
Renv$weighta<-c(0.5,1,1,1)
FLenv <- as.FL(Renv)

## # Simple example using optimal string alignment
## stringdist("ca","abc")
test_that("string distance",{
  result1=eval_expect_equal({test1<-stringdist("ca","abc")},Renv,FLenv)
  print(result1)
})

## # computing a 'dist' object
## d <- stringdistmatrix(c('foo','bar','boo','baz'))
## # try plot(hclust(d))
test_that("string distance",{
  result2=eval_expect_equal({test2<-stringdistmatrix(a)
  plot(hclust(test2))
  },Renv,FLenv)
  print(result2)
})

## # The following gives a matrix
## stringdistmatrix(c("foo","bar","boo"),c("baz","buz"))
test_that("string distance",{
  result3=eval_expect_equal({test3<-stringdistmatrix(b,c)},Renv,FLenv)
  print(result3)
})

## # An example using Damerau-Levenshtein distance (multiple editing of substrings allowed)
## stringdist("ca","abc",method="dl")
#DONE BELOW

## # string distance matching is case sensitive:
## stringdist("ABC","abc")
test_that("String distance",{
  result4=eval_expect_equal({test4<-stringdist("ABC","abc")},Renv,FLenv)
  print(result4)
})

## # so you may want to normalize a bit:
## stringdist(tolower("ABC"),"abc")
test_that("String distance",{
  result5=eval_expect_equal({test5<-stringdist(tolower("ABC"),"abc")},Renv,FLenv)
  print(result5)
})
     
## # stringdist recycles the shortest argument:
## stringdist(c('a','b','c'),c('a','c'))
test_that("string distance",{
  result6=eval_expect_equal({test6<-stringdist(d,e)},Renv,FLenv)
  print(result6)
})

     
## # stringdistmatrix gives the distance matrix (by default for optimal string alignment):
## stringdist(c('a','b','c'),c('a','c'))
test_that("string distance",{
  result7=eval_expect_equal({test7<-stringdist(d,e)},Renv,FLenv)
  print(result7)
})

## # different edit operations may be weighted; e.g. weighted substitution:
## stringdist('ab','ba',weight=c(1,1,1,0.5))
#ERROR: vector not supported in stringdist
test_that("string distance",{
  result8=eval_expect_equal({test8<-stringdist('ab','ba',weight)},Renv,FLenv)
  print(result8)
})

## # Non-unit weights for insertion and deletion makes the distance metric asymetric
## stringdist('ca','abc')
## stringdist('abc','ca')
## stringdist('ca','abc',weight=c(0.5,1,1,1))
## stringdist('abc','ca',weight=c(0.5,1,1,1))
#ERROR: vector not supported in stringdist
test_that("string distance",{
  result9=eval_expect_equal({
  test9<-stringdist('ca','abc')
  test10<-stringdist('abc','ca')
  test11<-stringdist('ca','abc',weighta)
  test12<-stringdist('abc','ca',weighta)
  },Renv,FLenv)
  print(result9)
})

## # Hamming distance is undefined for 
## # strings of unequal lengths so stringdist returns Inf
## stringdist("ab","abc",method="h")
## # For strings of eqal length it counts the number of unequal characters as they occur
## # in the strings from beginning to end
## stringdist("hello","HeLl0",method="h")
#DONE BELOW
     
## # The lcs (longest common substring) distance returns the number of 
## # characters that are not part of the lcs.
## #
## # Here, the lcs is either 'a' or 'b' and one character cannot be paired:
## stringdist('ab','ba',method="lcs")
## # Here the lcs is 'surey' and 'v', 'g' and one 'r' of 'surgery' are not paired
## stringdist('survey','surgery',method="lcs")
test_that("longest common substring distance", {
  result13=eval_expect_equal({
  test13<-stringdist('ab','ba',method="lcs")
  test14<-stringdist('survey','surgery',method="lcs")
  },Renv,FLenv)
  print(result13)
})

## # q-grams are based on the difference between occurrences of q consecutive characters
## # in string a and string b.
## # Since each character abc occurs in 'abc' and 'cba', the q=1 distance equals 0:
## stringdist('abc','cba',method='qgram',q=1)
test_that("String distance", {
  result15=eval_expect_equal({
  test15<-stringdist('ab','ba',method="lcs")
  test16<-stringdist('abc','cba',method='qgram',q=1)
  },Renv,FLenv)
  print(result15)
})

## # since the first string consists of 'ab','bc' and the second 
## # of 'cb' and 'ba', the q=2 distance equals 4 (they have no q=2 grams in common):
## stringdist('abc','cba',method='qgram',q=2)
test_that("String distance", {
  result16=eval_expect_equal({
  test16<-stringdist('ab','ba',method="lcs")
  test17<-stringdist('abc','cba',method='qgram',q=1)
  },Renv,FLenv)
  print(result16)
})

## # Wikipedia has the following example of the Jaro-distance. 
## stringdist('MARTHA','MATHRA',method='jw')
## # Note that stringdist gives a  _distance_ where wikipedia gives the corresponding 
## # _similarity measure_. To get the wikipedia result:
## 1 - stringdist('MARTHA','MATHRA',method='jw')
#DONE BELOW

## # The corresponding Jaro-Winkler distance can be computed by setting p=0.1
## stringdist('MARTHA','MATHRA',method='jw',p=0.1)
## # or, as a similarity measure
## 1 - stringdist('MARTHA','MATHRA',method='jw',p=0.1)
#DONE BELOW

## # This gives distance 1 since Euler and Gauss translate to different soundex codes.
## stringdist('Euler','Gauss',method='soundex')
## # Euler and Ellery translate to the same code and have distance 0
## stringdist('Euler','Ellery',method='soundex')
test_that("string distance", {
  result17=eval_expect_equal({
  test17<-stringdist('Euler','Gauss',method='soundex')
  test18<-stringdist('Euler','Ellery',method='soundex')
  },Renv,FLenv)
  print(result17)
})


test_that("Damerau-Levenshtein distance",{
result19=eval_expect_equal({
test19<-stringdist("ca","abc",method="dl")},Renv,FLenv)
print(result19)
})

test_that("Levenshtein distance",{
result20= eval_expect_equal({
test20<-stringdist("ca","abc",method="lv")},Renv,FLenv)
print(result20) 
})
 
 
test_that("Jaro-distance",{
result21=eval_expect_equal({
test21<-stringdist('MARTHA','MATHRA',method='jw')
test22<-(1 - stringdist('MARTHA','MATHRA',method='jw'))
},Renv,FLenv)
print(result21)
})

test_that("Jaccard distance" ,{
result23=eval_expect_equal({
test23<-stringdist('MARTHA','MATHRA',method='jaccard')},Renv,FLenv)
print(result23)
})

test_that("Jaro-Winkler distance", {
result24=eval_expect_equal({
test24<-stringdist('MARTHA','MATHRA',method='jw',p=0.1)
test25<-(1 - stringdist('MARTHA','MATHRA',method='jw',p=0.1))
},Renv,FLenv)
print(result24)
})

test_that("hamming distance", {
result26=eval_expect_equal({
test26<-stringdist("ab","abc",method="h")
test27<-stringdist("hello","HeLl0",method="h")
},Renv,FLenv)
print(result26)
})



