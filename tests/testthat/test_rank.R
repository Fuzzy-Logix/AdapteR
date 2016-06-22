Renv = new.env(parent = globalenv())

Renv$var1 = c(3, 1, 4, 15, 92)
Renv$var2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(Renv$var2) = letters[1:11]

FLenv = as.FL(Renv)

#Test failed .Giving different results for R and AdapteR.
#AdapteR is giving result like rankMatrix( i.e. it is giving number of independent vectors)
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360570
test_that("Check for rank function with default method average",{
          result = eval_expect_equal({
                   test1 = rank(var1)
                   test2 = rank(var2)
            },Renv,FLenv)
          print(result)
    })

test_that("Check for idempotent characteristic of rank function",{
          result = eval_expect_equal({
                   stopifnot(rank(test1) == test1, rank(test2) == test2)
            },Renv,FLenv)
          print(result)
    })


#Test failed . Error message - sort.list can be called on atomic list only.
#Asana ticket - https://app.asana.com/0/143316600934101/146934264360570
test_that("Check for different methods of rank function",{
          result = eval_expect_equal({
                   test3 = rank(var2, ties.method= "first")  # first occurrence wins
                   test4 = rank(var2, ties.method= "last")   #  last occurrence wins
                   test5 = rank(var2, ties.method= "random") # ties broken at random
                   test6 = rank(var2, ties.method= "random")
                   test7 = rank(var2, ties.method= "max")
                   test8 = rank(var2, ties.method= "min")
                   },Renv,FLenv)
          print(result)
    })

  
   
 

