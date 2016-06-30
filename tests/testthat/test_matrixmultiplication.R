#testing matrix multiplication
Renv <- new.env(parent = globalenv())
Renv$x<-1:4
Renv$y<-diag(Renv$x)
Renv$z<-matrix(1:12, ncol = 3, nrow = 4)
FLenv <- as.FL(Renv)
test_that( "Testing vectors",
           {
            result1=eval_expect_equal({
            test1<-x%*%x
             },Renv,FLenv)
             print(result1) 
             })
test_that( "Testing matrices",
           {
            result2=eval_expect_equal({
            test2<-y%*%z
             },Renv,FLenv)
             print(result2) 
           })
test_that( "Testing matrix & vector",
           {
            result3=eval_expect_equal({
            test3<-y%*%x
             },Renv,FLenv)
             print(result3) 
           })
test_that( "Testing vector & matrix",
           {
            result4=eval_expect_equal({
            test4<-x%*%z
             },Renv,FLenv)
             print(result4) 
           })



