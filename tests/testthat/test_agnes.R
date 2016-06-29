library(testthat)
Renv <- new.env(parent = globalenv())
Renv$data <- votes.repub
FLenv <- as.FL(Renv)

#asana ticket              https://app.asana.com/0/143316600934101/149542180184409
test_that("agnes",eval_expect_equal({
  agn1 <- agnes(votes.repub)    #, metric = "manhattan", stand = TRUE)
  order <- length(agn1$order)
  height <- length(agn1$height)
  merge <- dim(agn1$merge)
  diss_check <- agn1$diss
  },Renv,FLenv))


#To check for other clustering methods. There are others as well. Took one example.
# test_that("agnes",eval_expect_equal({
#   agnS <- agnes(votes.repub, method = "flexible", par.meth = 0.625)
#   orderS <- length(agnS$order)
#   heightS <- length(agnS$height)
#   mergeS <- dim(agnS$merge)
#   diss_checkS <- agnS$diss
# },Renv,FLenv))


   

