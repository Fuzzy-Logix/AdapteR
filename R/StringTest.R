#testing string functions
Renv <- new.env(parent = globalenv())

test_that(
  "Testing of string functions",
{
#Damerau-Levenshtein distance
eval_expect_equal({stringdist("ca","abc",method="dl")},Renv)

#Levenshtein distance
eval_expect_equal({stringdist("ca","abc",method="lv")},Renv)

#Jaro-distance
eval_expect_equal({stringdist('MARTHA','MATHRA',method='jw')},Renv)

#Jaccard distance
eval_expect_equal({stringdist('MARTHA','MATHRA',method='jaccard')},Renv)

#Jaro-Winkler distance
eval_expect_equal({stringdist('MARTHA','MATHRA',method='jw',p=0.1)},Renv)

#hamming distance
eval_expect_equal({stringdist("ab","abc",method="h")},Renv)
eval_expect_equal({stringdist("hello","HeLl0",method="h")},Renv)
}

)

