library(testthat)
Renv <- new.env(parent = globalenv())
Renv$data <- votes.repub
Flenv <- as.FL(Renv)
test_that("Kmeans returns objects correctly",{
  eval_expect_equal({
    dv <- diana(data, metric = "manhattan", stand = TRUE)
    order <- length(dv$order)
    height <- length(dv$height)
    dc <- dim(dv$merge)
    diss <- dv$diss
    order_lab <- length(dv$order.lab)
    dt <- dim(dv$data)
  },Renv,FLenv,
  noexpectation="dv")
})