
#Data objects initialisation.
Renv = new.env(parent = globalenv())
Renv$vector1 = sample(1:1000,600,replace = TRUE)
Renv$matrix1 = matrix(sample(1:1000,600,replace = TRUE), nrow = 200, ncol = 300, byrow = TRUE)
Renv$dataframe1 = data.frame(sample(1:1000,600,replace = TRUE), sample(1:1000,600,replace = TRUE))
FLenv = as.FL(Renv)

#Divide data object into n buckets.
#SQL Query taking too much time.
#Only Deep table is accepted by getDescStatsUDT.

test_that("Check for FLNtile function of AdapteR",{
    n = 10
    result1 = FLNtile(FLenv$vector1,n)
    ##    print(result1)
    result2 = FLNtile(FLenv$matrix1,n)
    ##    print(result2)
    result3 = FLNtile(FLenv$dataframe1,n)
    ##    print(result3)
    })
