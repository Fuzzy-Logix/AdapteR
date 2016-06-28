#Data objects initialisation.
Renv = new.env(parent = globalenv())
Renv$vector1 = sample(1:1000,600,replace = TRUE)
Renv$matrix1 = matrix(sample(1:1000,600,replace = TRUE), nrow = 200, ncol = 300, byrow = TRUE)
Renv$dataframe1 = data.frame(sample(1:1000,600,replace = TRUE), sample(1:1000,600,replace = TRUE))
FLenv = as.FL(Renv)

#Test for deviation with vector type objects .
#Used all the methods that can be applied to deviation function.
#One Mismatch occured due to rounding issue with median-abs method.
#Asana Ticket - https://app.asana.com/0/143316600934101/148451906885287

test_that("Check for deviation with FL and R vector object",{
          result = eval_expect_equal({
          test1 = deviation(vector1,method = "median-abs")
          test2 = deviation(vector1,method = "mean-abs")
          test3 = deviation(vector1,method = "mean-square")
            },Renv,FLenv)
          print(result)
    })

#Test for deviation with matrix type objects .
#Used all the methods that can be applied to deviation function.
#One Mismatch occured due to rounding issue with median-abs method.
#Asana Ticket - https://app.asana.com/0/143316600934101/148451906885287

test_that("Check for deviation with FL and R matrix object",{
          result = eval_expect_equal({
          test4 = deviation(matrix1,method = "median-abs")
          test5 = deviation(matrix1,method = "mean-abs")
          test6 = deviation(matrix1,method = "mean-square")
            },Renv,FLenv)
          print(result)
    })

#Test for deviation with dataframe type objects .
#Used all the methods that can be applied to deviation function.
#One Mismatch occured due to rounding issue with median-abs method.
#deviation needs Deep table as argument only.
#Asana Ticket - https://app.asana.com/0/143316600934101/148451906885287

test_that("Check for deviation with FL and R dataframe object",{
          result = eval_expect_equal({
          test7 = deviation(dataframe1,method = "median-abs")
          test8 = deviation(dataframe1,method = "mean-abs")
          test9 = deviation(dataframe1,method = "mean-square")
            },Renv,FLenv)
          print(result)
    })