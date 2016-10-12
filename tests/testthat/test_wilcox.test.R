
## Wilcox Signed-Rank Test
# Source:http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Nonparametric/BS704_Nonparametric6.html
Renv = new.env(parent = globalenv())
Renv$a <- c(85,70,40,65,80,75,55,20)
Renv$b <- c(75,50,50,40,20,65,40,25)
FLenv = as.FL(Renv)



test_that("wilcox.test: paired Wilcox Signed Rank Test: statistic and p-value ",{
  result = eval_expect_equal({
      t <- wilcox.test(a, b, paired = TRUE,correct=FALSE)
  },Renv,FLenv,
  verbose=T,
  tolerance = .000001
  )
}) 

##wilcox Signed-Rank Test:
#Source: https://www.wavemetrics.com/products/igorpro/dataanalysis/statistics/tests/statistics_pxp34.htm

                                        #Results Match

a <- c(
63.65,	64.86,
58.99,	50.22,
57.45,	59.33,
61.18,	59.86,
58.79,	60.35,
63.49,	59.71,
56.52,	59.22,
59.44,	59.39,
60.77,	64.95,
64.34,	56.42,
59.55,	59.56,
63.76,	54.82,
63.62,	60.74,
51.23,	60.23,
61.69,	60.95,
64.41,	61.6,
54.59,	64.9,
57.84,	59.73,
57.33,	60.36,
57.14,	60.12
)
Renv = new.env(parent = globalenv())
Renv$t <- a[c(TRUE,FALSE)]
Renv$q <- a[c(FALSE,TRUE)]
FLenv = as.FL(Renv)


test_that("wilcox.test: unpaired Wilcoxon Signed-Rank Test: correct p.value and statistic",{
  result = eval_expect_equal({
      z <- wilcox.test(t, q, paired = FALSE)
  },Renv,FLenv,
  tolerance = .000001
  )
}) 



#Mann-Whitney-Wilcoxon Test
# source: https://www.r-bloggers.com/wilcoxon-mann-whitney-rank-sum-test-or-test-u/
Renv = new.env(parent = globalenv())
Renv$a = c(6, 8, 2, 4, 4, 5)
Renv$b = c(7, 10, 4, 3, 5, 6)
FLenv = as.FL(Renv)


test_that("wilcox.test: unpaired Mann-Whitney Wilcoxon Test: correct p.value and statistic",{
  result = eval_expect_equal({
      t <- wilcox.test(a, b, paired = FALSE, correct = FALSE)
  },Renv,FLenv,
  #check.attributes=F,
  tolerance = .000001,
  verbose = F
  )
  }) 


                                        #Mann - Whitney Test
                                        #source:
#     https://www.wavemetrics.com/products/igorpro/dataanalysis/statistics/tests/statistics_pxp34.htm


new <- 
c(
60.83,	59.22,
63.24,	65.09,
72.33,	71.45,
62.21,	72.33,
66.59,	74.71,
61.64,	68.42,
73.68,
70.19
)

Renv = new.env(parent = globalenv())
Renv$a <- new[c(TRUE, FALSE)]
Renv$b <- new[c(FALSE, TRUE)]
Renv$b <- Renv$b[-7]
Renv$a[8] <- 70.19
FLenv = as.FL(Renv)


test_that("wilcox.test: unpaired Mann-Whitney Wilcoxon Test",{
  result = eval_expect_equal({
      q <- wilcox.test(a, b, paired = FALSE, correct = FALSE)
  },Renv,FLenv,
  ##check.attributes=F,
  tolerance = .000001,
  verbose = F
  )
  }) 
