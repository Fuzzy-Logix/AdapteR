Renv <- new.env(parent = globalenv())
Renv$x <- rbind(cbind(rnorm(10,0,0.5), 
            rnorm(10,0,0.5)),
            cbind(rnorm(15,5,0.5), 
            rnorm(15,5,0.5)))
FLenv <- as.FL(Renv)


test_that("Kmedoids",eval_expect_equal({
  pamx <- pam(x,4)
  medoids <- dim(pamx$medoids)
  id.med <- length(pamx$id.med)
  clust <- length(pamx$clustering)
  isolate <- pamx$isolation
  clusinfo <- dim(pamx$clusinfo)
  silinfo_width <- dim(pamx$silinfo$widths)
  diss <- pamx$diss
},Renv,FLenv,
expectation=c("medoids","id.med",
            "clust","isolate","clusinfo",
            "silinfo_width")
noexpectation = c("pamx")
))
