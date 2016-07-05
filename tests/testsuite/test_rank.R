Renv = new.env(parent = globalenv())

Renv$var1 = c(3, 1, 4, 15, 92)
Renv$var2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(Renv$var2) = letters[1:11]
Renv$vector1 = sample(1:100,60,replace = TRUE)
# Renv$matrix1 = matrix(sample(1:100,60,replace = TRUE), nrow = 20, byrow = TRUE)
# Renv$dataframe1 = data.frame(sample(1:100,60,replace = TRUE), sample(1:100,60,replace = TRUE))

FLenv = as.FL(Renv)

test_that("Check for rank function with default method average",{
          result = eval_expect_equal({
                   test1 = rank(var1)
                   test2 = rank(var2)
            },Renv,FLenv)
          ##print(result)
    })

## Methods specific to DB-Lytix, not in R
test_that("Check for rank function with DB Lytix supported method",{
    test3 = rank(FLenv$vector1,ties.method = "perc")
    ##    print(test3)
    test4 = rank(FLenv$vector1,ties.method = "duplicate")
    ##    print(test4)
    # test5 = rank(FLenv$matrix1,ties.method = "perc")
    # ##    print(test5)
    # test6 = rank(FLenv$matrix1,ties.method = "duplicate")
    # ##    print(test6)
    # test7 = rank(FLenv$dataframe1,ties.method = "perc")
    # ##    print(test7)
    # test8 = rank(FLenv$dataframe1,ties.method = "duplicate")
    ##    print(test8)
})