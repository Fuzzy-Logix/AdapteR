#Function not in R
#FLNeedleManWunschDist

test_that("testing the example written in FLStringFunctions",{
  widetable  <- FLTable("FL_DEMO", "iris", "rownames")
  flv <- widetable[1:10,"Species"]
  resultflvector1 <- FLNeedleManWunschDist("xyz",flv)
  resultflvector2 <- FLNeedleManWunschDist("xyz",flv,method="lv",caseFLag=1)
  resultflvector3 <- FLNeedleManWunschDist("xyz",flv,method="hamming",vlength=4)
  resultflmatrix1 <- FLNeedleManWunschDist(flv,flv,method="jw",p=1)
  resultflmatrix2 <- FLNeedleManWunschDist(c("xyz","juio"),flv,method="jw")
  print(resultflvector1)
  print(resultflvector2)
  print(resultflvector3)
  print(resultflmatrix1)
  print(resultflmatrix2)
    
  })


