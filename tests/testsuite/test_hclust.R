library(testthat)
Renv <- new.env(parent = globalenv())
Renv$data <- USArrests
rownames(Renv$data) <- 1:nrow(Renv$data)
FLenv <- as.FL(Renv)

FLenv$hc <- hclust(FLenv$data)
Renv$hc <- hclust(dist(Renv$data))

test_that("hclust",
    eval_expect_equal({
  merge <-  dim(hc$merge)
  height <- length(hc$height)
  order <- length(hc$order)
  label <- length(hc$labels)
  },Renv,FLenv,
  expectation=c("merge","height","order","label"))
)

