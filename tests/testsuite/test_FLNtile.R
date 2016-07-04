N <- 100
M <- 60
#Data objects initialisation.
Renv = new.env(parent = globalenv())
Renv$vector1 = sample(1:N,M,replace = TRUE)
Renv$matrix1 = matrix(sample(1:N,M,replace = TRUE), nrow = 20, byrow = TRUE)
Renv$dataframe1 = data.frame(a=sample(1:N,M,replace = TRUE),
                             b=sample(1:N,M,replace = TRUE))
FLenv = as.FL(Renv)

#Divide data object into n buckets.
#SQL Query taking too much time.
#Only Deep table is accepted by getDescStatsUDT.
##options(debugSQL=T)
test_that("FLNtile: returning correct number of values: https://app.asana.com/0/150173007236461/146668378502512",{
    n = 10
    result1 = FLNtile(FLenv$vector1,n)
    expect_equal(length(result1),length(FLenv$vector1))
    expect_equal(length(as.vector(result1)),length(FLenv$vector1))
    ##print(result1)
    ##result2 = FLNtile(FLenv$matrix1,n)
    ##print(result2)
    ##result3 = FLNtile(FLenv$dataframe1,n)
    ##print(result3)
})
