Renv = new.env(parent = globalenv())
Renv$var1 =  swiss
FLenv = as.FL(Renv)

#Done the following initialisation as object in R needs to be of glm or lm class
# And in AdapteR object needs to  be of FLTable class.
#Asana Ticket - https://app.asana.com/0/143316600934101/147523528458753
Renv$var1 = lm(Fertility ~ Education+Agriculture, data = swiss)


#Test failed as lm of AdapteR giving an error.
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360575
test_that("Check for step function",{
          result = eval_expect_equal({
                   test1 = step(object = var1,scope = list(lower = Fertility ~ Education+Agriculture,
                                                           upper =Fertility ~ Education+Agriculture+Examination+Catholic))
            },Renv,FLenv)
          print(result)
    })

#Test failed as lm is not working.
#Asana Ticket - https://app.asana.com/0/143316600934101/146934264360575
#AdapteR tempers colnames of input dataframe (e.g Infant.Mortality has been changed InfantMortality)
#Asana ticket - https://app.asana.com/0/143316600934101/147523528458761
test_that("Check for step function with different direction",{
          result = eval_expect_equal({
                   test2 = step(object = var1,scope = list(lower = Fertility ~ Education+Agriculture,
                                                           upper = Fertility ~ Education+Agriculture+Examination+Catholic),
                                direction = "backward")

                   test3 = step(object = var1,scope = list(lower = Fertility ~ Education+Agriculture,
                                                           upper = Fertility ~ Education+Agriculture+Examination+Catholic),
                                direction = "forward")

                   test4 = step(object = var1,scope = list(lower = Fertility ~ Education+Agriculture,
                                                           upper = Fertility ~ Education+Agriculture+Examination+Catholic),
                                direction = "both")
                   },Renv,FLenv)
          print(result)
    })


## Multinomial 
## Asana Ticket : https://app.asana.com/0/136555696724838/371749207621403/f
FLenv$var2 <- FLTable(getTestTableName("tblLogRegr"),"ObsID","VarID","Num_Val", whereconditions=c("ObsID < 5000","VarID<5"))

test_that("Check for step--multinomial",{

          s3 <- step(FLenv$var2, scope=list(upper=c("-1","0","1","2","3")),
                    direction = "backward", 
                    familytype="multinomial",pRefLevel=1)
          pred3 <-  predict(s3, type = "response")
          s4 <- step(FLenv$var2, scope=list(upper=c("1","2","3"),lower=c("2")), 
                    direction = "Fbackward",
                    familytype="multinomial",pRefLevel=1)
          pred4 <-  predict(s4, type = "response")
})