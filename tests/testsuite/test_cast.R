#testing cast function
#Use acast or dcast depending on vector/matrix/matrix/data frame output
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
test_that( "Testing acast",
{
  result1=eval_expect_equal({
  names(airquality) <- tolower(names(airquality))
  aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
  test1<-acast(aqm, day ~ month ~ variable)
  test2<-acast(aqm, month ~ variable, mean)
  test3<-acast(aqm, month ~ variable, mean)
  test4<-acast(aqm, month ~ variable, mean)
  test5<-acast(aqm, variable ~ month, mean, subset = .(variable == "ozone"))
  test6<-acast(aqm, variable ~ month, mean, subset = .(month == 5))
              },Renv,FLenv)
} )

test_that("Testing acast & dcast",
          {
          result2=eval_expect_equal({
          names(ChickWeight) <- tolower(names(ChickWeight))
          chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)
          test1<-dcast(chick_m, time ~ variable, mean) 
          test2<-dcast(chick_m, diet ~ variable, mean) 
          test3<-acast(chick_m, diet ~ time, mean)
          test4<-acast(chick_m, time ~ diet, length)
          test5<-acast(chick_m, chick ~ time, mean)
          test6<-acast(chick_m, chick ~ time, mean, subset = .(time < 10 & chick < 20))
          test7<-acast(chick_m, time ~ diet, length)
          test8<-dcast(chick_m, diet + chick ~ time)
          test9<-acast(chick_m, diet + chick ~ time)
          test10<-acast(chick_m, chick ~ time ~ diet)
          test11<-acast(chick_m, diet + chick ~ time, length, margins="diet")
          test12<-acast(chick_m, diet + chick ~ time, length, drop = FALSE)
          },Renv,FLenv)
  })

test_that("Testing acast & dcast",
          {
            result2=eval_expect_equal({
              test1<-dcast(melt(tips), sex ~ smoker, mean, subset = .(variable == "total_bill"))
              ff_d <- melt(french_fries, id=1:4, na.rm=TRUE)
              test2<-acast(ff_d, subject ~ time, length)
              test3<-acast(ff_d, subject ~ time, length, fill=0)
              test4<-dcast(ff_d, treatment ~ variable, mean, margins = TRUE)
              test5<-dcast(ff_d, treatment + subject ~ variable, mean, margins="treatment")
            },Renv,FLenv)
          })




