Renv = new.env(parent = globalenv())

Renv$var1 = c(3, 1, 4, 15, 92)
Renv$var2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(Renv$var2) = letters[1:11]
Renv$vector1 = sample(1:1000,600,replace = TRUE)
Renv$matrix1 = matrix(sample(1:1000,600,replace = TRUE), nrow = 200, ncol = 300, byrow = TRUE)
Renv$dataframe1 = data.frame(sample(1:1000,600,replace = TRUE), sample(1:1000,600,replace = TRUE))

FLenv = as.FL(Renv)

test_that("Check for rank function with default method average",{
          result = eval_expect_equal({
                   test1 = rank(var1)
                   test2 = rank(var2)
            },Renv,FLenv)
          print(result)
    })

#AdapteR supports perc and frac methods other than average method for tie breaking.
#Even after masking rank function of R in AdapteR , it is not working for R objects.
#SQL Query takes lot of time.
test_that("Check for rank function with DB Lytix supported method",{
                   test3 = rank(FLenv$vector1,ties.method = "perc")
                   print(test3)
                   test4 = rank(FLenv$vector1,ties.method = "duplicate")
                   print(test4)
                   test5 = rank(FLenv$matrix1,ties.method = "perc")
                   print(test5)
                   test6 = rank(FLenv$matrix1,ties.method = "duplicate")
                   print(test6)
                   test7 = rank(FLenv$dataframe1,ties.method = "perc")
                   print(test7)
                   test8 = rank(FLenv$dataframe1,ties.method = "duplicate")
                   print(test8)
          
    })

test_that("Check for idempotent characteristic of rank function",{
          result = eval_expect_equal({
                   stopifnot(rank(test1) == test1, rank(test2) == test2)
            },Renv,FLenv)
          print(result)
    })


## fail..
#Asana ticket - https://app.asana.com/0/143316600934101/146934264360570

## All these methods are not supported.. DB-Lytix and  R have average.
## In addition FL has "perc" and "duplicate" which are not in R
## documentation

# test_that("Check for different methods of rank function",{
#           result = eval_expect_equal({
#                    test3 = rank(var2, ties.method= "first")  # first occurrence wins
#                    test4 = rank(var2, ties.method= "last")   #  last occurrence wins
#                    test5 = rank(var2, ties.method= "random") # ties broken at random
#                    test6 = rank(var2, ties.method= "random")
#                    test7 = rank(var2, ties.method= "max")
#                    test8 = rank(var2, ties.method= "min")
#                    },Renv,FLenv)
#           print(result)
#     })

  
   
 

