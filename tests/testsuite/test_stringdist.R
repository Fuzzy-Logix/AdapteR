## gk @richa: please implement tests for the following stringdist::stringdist examples

     ## # Simple example using optimal string alignment
     ## stringdist("ca","abc")
     
     ## # computing a 'dist' object
     ## d <- stringdistmatrix(c('foo','bar','boo','baz'))
     ## # try plot(hclust(d))
     
     ## # The following gives a matrix
     ## stringdistmatrix(c("foo","bar","boo"),c("baz","buz"))
     
     ## # An example using Damerau-Levenshtein distance (multiple editing of substrings allowed)
     ## stringdist("ca","abc",method="dl")
     
     ## # string distance matching is case sensitive:
     ## stringdist("ABC","abc")
     
     ## # so you may want to normalize a bit:
     ## stringdist(tolower("ABC"),"abc")
     
     ## # stringdist recycles the shortest argument:
     ## stringdist(c('a','b','c'),c('a','c'))
     
     ## # stringdistmatrix gives the distance matrix (by default for optimal string alignment):
     ## stringdist(c('a','b','c'),c('a','c'))
     
     ## # different edit operations may be weighted; e.g. weighted substitution:
     ## stringdist('ab','ba',weight=c(1,1,1,0.5))
     
     ## # Non-unit weights for insertion and deletion makes the distance metric asymetric
     ## stringdist('ca','abc')
     ## stringdist('abc','ca')
     ## stringdist('ca','abc',weight=c(0.5,1,1,1))
     ## stringdist('abc','ca',weight=c(0.5,1,1,1))
     
     ## # Hamming distance is undefined for 
     ## # strings of unequal lengths so stringdist returns Inf
     ## stringdist("ab","abc",method="h")
     ## # For strings of eqal length it counts the number of unequal characters as they occur
     ## # in the strings from beginning to end
     ## stringdist("hello","HeLl0",method="h")
     
     ## # The lcs (longest common substring) distance returns the number of 
     ## # characters that are not part of the lcs.
     ## #
     ## # Here, the lcs is either 'a' or 'b' and one character cannot be paired:
     ## stringdist('ab','ba',method="lcs")
     ## # Here the lcs is 'surey' and 'v', 'g' and one 'r' of 'surgery' are not paired
     ## stringdist('survey','surgery',method="lcs")
     
     
     ## # q-grams are based on the difference between occurrences of q consecutive characters
     ## # in string a and string b.
     ## # Since each character abc occurs in 'abc' and 'cba', the q=1 distance equals 0:
     ## stringdist('abc','cba',method='qgram',q=1)
     
     ## # since the first string consists of 'ab','bc' and the second 
     ## # of 'cb' and 'ba', the q=2 distance equals 4 (they have no q=2 grams in common):
     ## stringdist('abc','cba',method='qgram',q=2)
     
     ## # Wikipedia has the following example of the Jaro-distance. 
     ## stringdist('MARTHA','MATHRA',method='jw')
     ## # Note that stringdist gives a  _distance_ where wikipedia gives the corresponding 
     ## # _similarity measure_. To get the wikipedia result:
     ## 1 - stringdist('MARTHA','MATHRA',method='jw')
     
     ## # The corresponding Jaro-Winkler distance can be computed by setting p=0.1
     ## stringdist('MARTHA','MATHRA',method='jw',p=0.1)
     ## # or, as a similarity measure
     ## 1 - stringdist('MARTHA','MATHRA',method='jw',p=0.1)
     
     ## # This gives distance 1 since Euler and Gauss translate to different soundex codes.
     ## stringdist('Euler','Gauss',method='soundex')
     ## # Euler and Ellery translate to the same code and have distance 0
     ## stringdist('Euler','Ellery',method='soundex')
     



##testing string distance functions
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)

test_that(
  "Damerau-Levenshtein distance",
{
result1=eval_expect_equal({test1<-stringdist("ca","abc",method="dl")},Renv,FLenv)
##print(result1)
})

test_that("Levenshtein distance",{
result2= eval_expect_equal({test2<-stringdist("ca","abc",method="lv")},Renv,FLenv)
##print(result2) 
})
 
 
test_that("Jaro-distance",{
result3=eval_expect_equal({test3<-stringdist('MARTHA','MATHRA',method='jw')},Renv,FLenv)
##print(result3)
})

test_that("Jaccard distance" ,{
result4=eval_expect_equal({test4<-stringdist('MARTHA','MATHRA',method='jaccard')},Renv,FLenv)
##print(result4)
})

test_that("Jaro-Winkler distance", {
result5=eval_expect_equal({test5<-stringdist('MARTHA','MATHRA',method='jw',p=0.1)},Renv,FLenv)
##print(result5)
})

test_that("hamming distance", {
result6=eval_expect_equal({test6<-stringdist("ab","abc",method="h")},Renv,FLenv)
result7=eval_expect_equal({test7<-stringdist("hello","HeLl0",method="h")},Renv,FLenv)
##print(result6)
##print(result7)
})



