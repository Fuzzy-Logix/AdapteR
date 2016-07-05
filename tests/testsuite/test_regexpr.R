Renv = new.env(parent = globalenv())

Renv$text2 = c("arm","foot","lefroo", "bafoobar")

FLenv = as.FL(Renv)

test_that("Check for regexpr function with pattern of string type for text2 ",{
          result = eval_expect_equal({
                   test2 = regexpr("foo", text2)
                },Renv,FLenv,check.attributes = FALSE)
          ##          print(result)
    })
 
