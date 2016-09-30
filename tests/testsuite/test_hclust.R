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
    expectation=c("merge","height","order","label")))


## gk:  please save a left/right plot for checking, containing:
if(FALSE){
    opar <- par(mfrow = c(1, 2))
    plot(Renv$hc)
    plot(FLenv$hc)
    par(opar)
}

