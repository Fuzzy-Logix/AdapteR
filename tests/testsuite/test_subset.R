Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
test_that(
  "Testing subset",
  {
    result1=eval_expect_equal({test1<-subset(airquality, Temp > 80, select = c(Ozone, Temp))},Renv,FLenv)
    print(result1)
    
  })

test_that(
  "Testing subset",
  {
    result2=eval_expect_equal({test2<-subset(airquality, Day == 1, select = -Temp)},Renv,FLenv)
    print(result2)
    
  })

test_that(
  "Testing subset",
  {
    result3=eval_expect_equal({test3<-subset(airquality, select = Ozone:Wind)},Renv,FLenv)
    print(result3)
    
  })

test_that(
  "Testing subset",
  {
    result4=eval_expect_equal({
    nm <- rownames(state.x77)
    start_with_M <- nm %in% grep("^M", nm, value = TRUE)
    test4<-subset(state.x77, start_with_M, Illiteracy:Murder)},Renv,FLenv)
    print(result4)
    
  })


test_that(
  "Testing subset",
  {
    result5=eval_expect_equal({
      nm <- rownames(state.x77)
      start_with_M <- nm %in% grep("^M", nm, value = TRUE)
      test5<-subset(state.x77, grepl("^M", nm), Illiteracy:Murder)},Renv,FLenv)
    print(result5)
    
  })



