Renv = new.env(parent = globalenv())

Renv$var1 = c(18,17,15,20,10,20,25,13,12)
Renv$var2 = gl(3,1,9)
Renv$var3 = gl(3,3)
Renv$var4 = data.frame(variable3 = Renv$var3,variable2 = Renv$var2,variable1 = Renv$var1)

utils::data(anorexia6, package = "MASS")
Renv$var5 = anorexia

Renv$var6 = data.frame(
            u = c(5,10,15,20,30,40,60,80,100),
            lot1 = c(118,58,42,35,27,25,21,19,18),
            lot2 = c(69,35,26,21,18,16,13,12,12))

FLenv= as.FL(Renv)

#Incorrect number of dimensions .
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for glm function with family to be default ( gaussian in R) and (binomial in AdapteR)",{
          result = eval_expect_equal({
                   test1 =  glm(variable1 ~ variable2 + variable3,data = var4)
            },Renv,FLenv)
          print(result)
    })

#Incorrect Number of dimensions.
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for glm function with family to be default",{
          result = eval_expect_equal({
                   test2 = glm(Postwt ~ Prewt + Treat + offset(Prewt),
                               data = var5)
            },Renv,FLenv)
          print(result)
    })

#Test for glm with family Gamma.
#Not supported for gamma family.
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for glm function with Gamma family",{
          result = eval_expect_equal({
                   test3 = glm(lot1 ~ log(u), data = var6, family = Gamma)
                   test4 = glm(lot2 ~ log(u), data = var6, family = Gamma)
            },Renv,FLenv)
          print(result)
    })
