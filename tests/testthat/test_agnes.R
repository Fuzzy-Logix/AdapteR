library(testthat)
Renv <- new.env(parent = globalenv())
Renv$data <- votes.repub
#Renv$d.vr <- daisy(votes.repub)    #dissimilarity matrix
Renv$animals <- animals
FLenv <- as.FL(Renv)

#asana ticket              https://app.asana.com/0/143316600934101/149542180184409
test_that("agnes",eval_expect_equal({
  agn1 <- agnes(votes.repub)    #, metric = "manhattan", stand = TRUE)
  order <- length(agn1$order)
  height <- length(agn1$height)
  merge <- dim(agn1$merge)
  diss_check <- agn1$diss
  },Renv,FLenv,
  noexpectations=c("agn1")
  )
)


#To check for other clustering methods. There are others as well. Took one example.
# test_that("agnes",eval_expect_equal({
#   agnS <- agnes(votes.repub, method = "flexible", par.meth = 0.625)
#   orderS <- length(agnS$order)
#   heightS <- length(agnS$height)
#   mergeS <- dim(agnS$merge)
#   diss_checkS <- agnS$diss
# },Renv,FLenv))

#test for dissimilarity matrix      method="single" or method="complete" is also applied in R examples
#test_that("agnes",eval_expect_equal({
  #   a.wgt  <- agnes(d.vr, method = "weighted")
  #   orderW <- length(a.wgt$order)
  #   heightW <- length(a.wgt$height)
  #   mergeW <- dim(a.wgt$merge)
  #   diss_checkW <- a.wgt$diss
  # },Renv,FLenv,
  #noexpectations=c("a.wgt")
#))
  

 test_that("agnes gaverage",eval_expect_equal({
    aa.ga <- agnes(animals, method = "gaverage")
    orderGa <- length(aa.ga$order)
    heightGa <- length(aa.ga$height)
    mergeGa <- dim(aa.ga$merge)
    diss_checkGa <- aa.ga$diss
  },Renv,FLenv,
 noexpectations=c("aa.ga")
 ))





   

