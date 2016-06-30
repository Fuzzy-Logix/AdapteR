#Data objects initialisation.
Renv = new.env(parent = globalenv())
Renv$vector1 = sample(1:1000,600,replace = TRUE)
Renv$matrix1 = matrix(sample(1:1000,600,replace = TRUE), nrow = 200, ncol = 300, byrow = TRUE)
Renv$dataframe1 = data.frame(x=sample(1:1000,600,replace = TRUE),
                             y=sample(1:1000,600,replace = TRUE))

FLenv = as.FL(Renv)

##Test for deviation with vector type objects .
##Used all the methods that can be applied to deviation function.
##One Mismatch occured due to rounding issue with median-abs method.
##Asana Ticket - https://app.asana.com/0/143316600934101/148451906885287
test_that("deviation vector",{
    result = eval_expect_equal({
        devmeansquare = deviation(vector1,method = "mean-square")
        devmeanabs = deviation(vector1,method = "mean-abs")
        devmedianabs = deviation(vector1,method = "median-abs")
    },Renv,FLenv,
    expectation = c("devmeansquare","devmeanabs","devmedianabs"))
    ##print(result)
})

test_that("deviation matrix",{
    result = eval_expect_equal({
        devmeansquare = deviation(matrix1,method = "mean-square")
        devmeanabs = deviation(matrix1,method = "mean-abs")
        devmedianabs = deviation(matrix1,method = "median-abs")
    },Renv,FLenv,
    expectation = c("devmeansquare","devmeanabs","devmedianabs"))
    print(result)
})

test_that("deviation data.frame",{
    result = eval_expect_equal({
        devmeansquare = deviation(dataframe1,method = "mean-square")
        devmeanabs = deviation(dataframe1,method = "mean-abs")
        devmedianabs = deviation(dataframe1,method = "median-abs")
    },Renv,FLenv,
    expectation = c("devmeansquare","devmeanabs","devmedianabs"))
    ##print(result)
})


#Test for deviation with matrix type objects .
#Used all the methods that can be applied to deviation function.
#One Mismatch occured due to rounding issue with median-abs method.
#Asana Ticket - https://app.asana.com/0/143316600934101/148451906885287


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
          },Renv,FLenv,
          expectation = paste0("test",7:9))
          ##print(result)
    })
