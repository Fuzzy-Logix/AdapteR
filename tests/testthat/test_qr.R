#No signature for function in as.FL.Thus, defined function in environment other than R environment.
# Asana Ticket = https://app.asana.com/0/143316600934101/144942913968262
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h9 <- hilbert(9)

Renv = new.env(parent = globalenv())

Renv$mat1 = h9

#Test failed.
#Getting different results for R and FL function
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968308
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test1 = qr(mat1)$rank
                                },Renv)
    print(result)
    })

test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test2 = qr(mat1,tol = 1e-10)$rank
                                },Renv)
    print(result)
    })

Renv$mat2 = 1:9/10

#Test Failed
#Needs different arguments needed as first argument for R and FL qr.solve.
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968316
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test3 = qr.solve(mat1,mat2,tol = 1e-10)
                                },Renv)
    print(result)
    })

#Test Failed
#Needs different arguments needed as first argument for R and FL qr.solve.
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968316
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test4 = qr.coef(qr(mat1,tol = 1e-10), mat2)
                                },Renv)
    print(result)
    })
 
Renv$mat3 = matrix(runif(12), 4)
Renv$mat4 =  1:4

#Test failed.
#Overdetermined function.
#solve needs a numeric matrix as first argument in FL solve function.
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968316
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test5 = solve(qr(mat3),mat4)
                                },Renv)
    print(result)
    })

Renv$mat5 = matrix(runif(12), 3)
Renv$mat6 = 1:3

#underdetermined function
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test6 = solve(mat5,mat6)
                                },Renv)
    print(result)
    })