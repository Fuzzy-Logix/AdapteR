Renv = new.env(parent = globalenv())

Renv$mat1 = matrix(rnorm(100), nrow = 5)
Renv$var2 =  c(0, 0, 1, 1, 1, 1)
Renv$var3 =  c(1, 0, 1, 1, 0, 1)

FLenv = as.FL(Renv)


test_that("Check for matrix distance with different arguments",{
           result = eval_expect_equal({ test1 = dist(mat1)
                               test2 = dist(mat1, diag = TRUE)
                               test3 = dist(mat1, upper = TRUE)
                               test10 = dist(mat1, method="manhattan")
                               test11 = dist(mat1,diag=T,upper=T,method="euclidean")
                               },Renv,FLenv,check.attributes=FALSE)
           print(result)
    })


# rbind function is not working. Error coming No method for coercing S4 class to vector.
## casting with Inf does not work.https://app.asana.com/0/143316600934101/146934264360563
## only manhattan and euclidean methods are in DB-Lytix.

# #Asana Ticket = https://app.asana.com/0/143316600934101/145657030318443
# test_that("Check for matrix distance with binary method and Canberra method",{
#            result = eval_expect_equal({ test4 = dist(rbind(var2,var3),method = "binary")
#                                         test5 = dist(rbind(var2,var3),method = "canberra")
#                                        },Renv,FLenv)
#            print(result)
#     })

# #Initialisation done here to avoid problem of running rbind in FLenv.

# (Renv$var3)[6] = Inf
# (Renv$var2)[6] = Inf
# Renv$mat2 = rbind(var2,var3)
# FLenv = as.FL(Renv)

# #Tests with rows having Infinity as one element.


# #Not supported for methods other than Eucledian and Manhattan
# test_that("Check for matrix distance with binary ,maximum,Canberra method",{
#            result = eval_expect_equal({test6 = dist(mat2,method = "binary")
#                                         test7 = dist(mat2,method = "canberra")
#                                         test8= dist(rbind(x, y), method = "maximum") 
#                                        },Renv,FLenv)
#            print(result)
#     })


# #Tets Failed.Sql error and error was Not yet implemented for matrix with NULL values.
# #Asana Ticket = https://app.asana.com/0/143316600934101/145657030318462
# test_that("Check for matrix distance with binary ,maximum,Canberra method",{
#            result = eval_expect_equal({test9 = dist(mat2,method = "manhattan")
#                                        },Renv,FLenv)
#            print(result)
#     })
