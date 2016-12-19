## FLPearsonChiSq still doesn't function, asana- https://app.asana.com/0/150173007236461/182190129148838
## I've used the examples from R documentation

Renv=new.env(parent=globalenv())
Renv$M <- rbind(c(762, 327, 468), c(484, 239, 477))
dimnames(Renv$M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
Renv$x <- matrix(c(12, 5, 7, 7), ncol = 2)
FLenv<-as.FL(Renv)
##issue with data frame to table conversion

test_that("test for pearson chi square on a table",{
	r<-chisq.test(Renv$M)
	fl<-chisq.test(FLenv$M,pear=1)
	FLexpect_equal(as.numeric(r[["p.value"]]),as.numeric(fl[["p.value"]]))
	})

test_that("test for pearson chi square on a matrix",{
	r<-chisq.test(Renv$x)
	fl<-chisq.test(FLenv$x,pear=1)
	FLexpect_equal(as.numeric(r[["p.value"]]),as.numeric(fl[["p.value"]]))
	})
