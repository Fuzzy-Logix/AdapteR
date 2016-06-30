library(testthat)
Renv <- new.env(parent = globalenv())
Renv$x <- rbind(cbind(rnorm(10, 0, 0.5), rnorm(10, 0, 0.5)),
           cbind(rnorm(15, 5, 0.5), rnorm(15, 5, 0.5)),
           cbind(rnorm( 3,3.2,0.5), rnorm( 3,3.2,0.5)))
FLenv <- as.FL(Renv)


test_that("FLFKMeans",eval_expect_equal({
  fannyx <- fanny(x, 2)
  memb_exp <- fannyx$memb.exp
  clust <- length(fannyx$clustering)
  k_crisp <- fannyx$k.crisp
  silinfo_width <- dim(fannyx$silinfo$widths)
  diss <- fannyx$diss
},Renv,FLenv,
expectation=c("memb_exp","clust","k_crisp",
            "silinfo_width","diss"),
noexpectation = c("fannyx")
))
