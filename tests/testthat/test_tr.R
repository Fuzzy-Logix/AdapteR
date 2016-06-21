Renv = new.env(parent = globalenv())

Renv$mat1 = matrix(1:16,ncol=4)


test_that("Check for trace function ",{
    result = eval_expect_equal({test1 = tr(mat1)
                                },Renv)
    print(result)
    })
