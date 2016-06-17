pm <- as(readMM(system.file("external/pores_1.mtx",
                            package = "Matrix")),
         "CsparseMatrix")
Renv = new.env(parent = globalenv())

#dgeMatrix
Renv$mat1 = Matrix(rnorm(9), 3, 3)

Renv$mat2 = pm
FLenv <- as.FL(Renv)

#Test failed due to types not compatible.
#Results of slot x was same.
#No inherited method for dgeMatrix and denseLU in as.FL function.
#Asana Ticket - https://app.asana.com/0/143316600934101/145318689357916
#resolved
test_that("Check for LU Decomposition function ",{
    result = eval_expect_equal({
        test1 = (lu(mat1))@x},
        Renv,FLenv)
    print(result)
    })

#Test failed
#results were also different.
 #no method for coercing this S4 class to a vector
test_that("Check for LU Decomposition function ",{
    result = eval_expect_equal({
        test2 = (lu(mat2))@x},
        Renv,FLenv)
    print(result)
    })
