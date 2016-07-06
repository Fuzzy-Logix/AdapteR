Renv = new.env(parent = globalenv())

Renv$var1 = c(3, 1, 4, 15, 92)
Renv$var2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(Renv$var2) = letters[1:11]
Renv$vector1 = sample(1:100,60,replace = TRUE)

FLenv = as.FL(Renv)

test_that("rank: default method average, named vectors",{
    result = eval_expect_equal({
        test1 = rank(var1)
        test2 = rank(var2)
    },Renv,FLenv,
    expectation=c("test1","test2"))
    ##print(result)
})

test_that("rank: check for idempotent characteristic of rank function with ties.method=perc, duplicate",{
    result = eval_expect_equal({
        rtest1 <- rank(test1)
        expect_equal(as.vector(rtest1),as.vector(test1))
        rtest2 <- rank(test2)
        expect_equal(as.vector(rtest2),as.vector(test2))
    },Renv,FLenv)
    ##print(result)
})


#AdapteR supports perc and frac methods other than average method for tie breaking.
#SQL Query takes lot of time.
test_that("rank(FLVector): ",{
    test3 = rank(FLenv$vector1,ties.method = "perc")
    expect_equal(as.vector(rank(test3,ties.method = "perc")),
                 as.vector(test3))
    test4 = rank(FLenv$vector1,ties.method = "duplicate")
    expect_equal(as.vector(rank(test4,ties.method = "duplicate")),
                 as.vector(test4))
})


  
   
 

