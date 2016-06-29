
## Data objects initialisation.
Renv = new.env(parent = globalenv())
Renv$vector1 = sample(1:1000,600,replace = TRUE)
Renv$matrix1 = matrix(sample(1:1000,600,replace = TRUE), nrow = 200, ncol = 300, byrow = TRUE)
Renv$dataframe1 = data.frame(sample(1:1000,600,replace = TRUE), sample(1:1000,600,replace = TRUE))
FLenv = as.FL(Renv)

## Checks equality between mode of FL vector and R vector.
## For R vector mode is calculated using count of plyr package.
## Test Failed : For FL object mode is returning all possible mode and 
## for R object mode is returning first possible mode only.
## Asana Ticket - https://app.asana.com/0/143316600934101/148420141220850
test_that("Check for mode with FL and R vector object",{
  result = eval_expect_equal({
    test1 = mode(vector1)
                                        #print(paste0("Result for ",environment()," is :",test1))
                                        #Not able to use environment() in FLenv.
  },Renv,FLenv)
  ##print(result)
})

## Test failed as for R objects mode is giving just one pssible mode.
## And for FL object all possible modes are displayed.
## Asan Ticket - https://app.asana.com/0/143316600934101/148420141220850 
test_that("Check for mode with FL and R matrix object",{
  result = eval_expect_equal({
    test2 = mode(matrix1)
  },Renv,FLenv)
  ##print(result)
})

## Test failed as getDescStatsUdt needs deep table.
## Asana Ticket - https://app.asana.com/0/143316600934101/148420141220860
test_that("Check for mode with FL and R dataframe object",{
  result = eval_expect_equal({
    test3 = mode(dataframe1)
  },Renv,FLenv)
  ##print(result)
})

