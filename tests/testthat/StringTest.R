#testing string distance functions
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)

test_that(
  "Damerau-Levenshtein distance",
{
result1=eval_expect_equal({test1<-stringdist("ca","abc",method="dl")},Renv,FLenv)
print(result1)
})

test_that("Levenshtein distance",{
result2= eval_expect_equal({test2<-stringdist("ca","abc",method="lv")},Renv,FLenv)
print(result2) 
})
 
 
test_that("Jaro-distance",{
result3=eval_expect_equal({test3<-stringdist('MARTHA','MATHRA',method='jw')},Renv,FLenv)
print(result3)
})

test_that("Jaccard distance" ,{
result4=eval_expect_equal({test4<-stringdist('MARTHA','MATHRA',method='jaccard')},Renv,FLenv)
print(result4)
})

test_that("Jaro-Winkler distance", {
result5=eval_expect_equal({test5<-stringdist('MARTHA','MATHRA',method='jw',p=0.1)},Renv,FLenv)
print(result5)
})

test_that("hamming distance", {
result6=eval_expect_equal({test6<-stringdist("ab","abc",method="h")},Renv,FLenv)
result7=eval_expect_equal({test7<-stringdist("hello","HeLl0",method="h")},Renv,FLenv)
print(result6)
print(result7)
})



