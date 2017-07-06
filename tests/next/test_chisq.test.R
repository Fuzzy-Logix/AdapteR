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
	FLexpect_equal(as.numeric(r[["p.value"]]),
                    as.numeric(fl[["p.value"]]))
	})

test_that("test for pearson chi square on a matrix",{
	r<-chisq.test(Renv$x)
	fl<-chisq.test(FLenv$x,pear=1)
	FLexpect_equal(as.numeric(r[["p.value"]]),
                as.numeric(fl[["p.value"]]))
	})

test_that("test for chi square on a matrix",{
	fl <- chisq.test(FLenv$M)
	chi_sq <- data.frame(c(4.834967, 6.273369), c(0.1692254, 0.2195700),
						c(8.084012, 10.489006))
	colnames(chi_sq) <- c("Democrat", "Independent", "Republican")
	rownames(chi_sq) <- c("F", "M")

	exp_val <- data.frame(c(703.6714,  542.3286), c(319.6453, 246.3547),
						c(533.6834, 411.3166))
	colnames(exp_val) <- c("Democrat", "Independent", "Republican")
	rownames(exp_val) <- c("F", "M")

	expect_equal(fl$chi_sq, chi_sq, tolerance= 1e-5)
	expect_equal(fl$exp_val, exp_val, tolerance= 1e-5)
	})