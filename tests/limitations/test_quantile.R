Renv = new.env(parent = globalenv())
Renv$prob1 = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100

FLenv = as.FL(Renv)
#test failed . handling NA and NULLS 
## in FL Cast Functions and FL Matrix Arithematic
#Asana Ticket - ## https://app.asana.com/0/143778401455745/146934264360563
test_that("Check for quantile function",{
          result = eval_expect_equal({
                   test10 = quantile(var1)
                   test11 = quantile(var1,probs = prob1)
            },Renv,FLenv)
          ##          print(result)
    })