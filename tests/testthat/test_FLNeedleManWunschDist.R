#Function not in R
#FLNeedleManWunschDist

test_that("testing FLNeedleManWunschDist working ",{
  flv <- as.FLVector(c("foo","bar"))
  result1 <- stringdist(c("xyz","juio"),flv,method="nmw")
  result1 <- stringdist(flv,c("xyz","juio"),method="nmw",weight=c(d=1))
  result1 <- stringdist(flv,flv,method="nmw",weight=c(d=1,i=-1))
  result1 <- stringdist(c("xyz","juio"),flv,method="nmw",weight=c(d=1,i=-1,s=-1))
  })
