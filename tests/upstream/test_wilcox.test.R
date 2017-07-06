
## Wilcox Signed-Rank Test
## Source:http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Nonparametric/BS704_Nonparametric6.html
Renv = new.env(parent = globalenv())
Renv$a <- c(85,70,40,65,80,75,55,20)
Renv$b <- c(75,50,50,40,20,65,40,25)
FLenv = as.FL(Renv)


# pValues have a difference of 0.01
test_that("wilcox.test: paired Wilcox Signed Rank Test: statistic and p-value ",{
  result = eval_expect_equal({
      res <- wilcox.test(a, b, paired = TRUE,correct=FALSE)
      statistic <- res$statistic
      pValue <- res$p.value
      names(statistic) <- NULL
  },Renv,FLenv,
  verbose=T,
  expectation = c("statistic", "pValue"),
  noexpectation = c("res"),
  tolerance = .01,
  scale = 1
  )
}) 

## wilcox Signed-Rank Test:
## Source: https://www.wavemetrics.com/products/igorpro/dataanalysis/statistics/tests/statistics_pxp34.htm
## Results Match

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

# Difference of 0.02 in p.value
test_that("wilcox.test: unpaired Wilcoxon Signed-Rank Test: correct p.value and statistic",{
  result = eval_expect_equal({
      res <- wilcox.test(t, q, paired = FALSE)
      statistic <- res$statistic
      pValue <- res$p.value
      names(statistic) <- NULL
  },Renv,FLenv,
  verbose=T,
  expectation = c("statistic", "pValue"),
  noexpectation = c("res"),
  tolerance = .02
  )
}) 

## Mann-Whitney-Wilcoxon Test
## source: https://www.r-bloggers.com/wilcoxon-mann-whitney-rank-sum-test-or-test-u/
Renv = new.env(parent = globalenv())
Renv$a = c(6, 8, 2, 4, 4, 5)
Renv$b = c(7, 10, 4, 3, 5, 6)
FLenv = as.FL(Renv)
test_that("wilcox.test: unpaired Mann-Whitney Wilcoxon Test: correct p.value and statistic",{
  result = eval_expect_equal({
      t <- wilcox.test(a, b, paired = FALSE, correct = FALSE)
      statistic <- t$statistic
      pValue <- t$p.value
      names(statistic) <- NULL
  },Renv,FLenv,
  verbose = T,
  expectation = c("statistic", "pValue"),
  noexpectation = c("t"),
  tolerance = .01,
  scale = 1
  )
}) 


## Mann - Whitney Test
## source:
## https://www.wavemetrics.com/products/igorpro/dataanalysis/statistics/tests/statistics_pxp34.htm
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

# Difference of 0.03 in p.value
test_that("wilcox.test: unpaired Mann-Whitney Wilcoxon Test ",{
  result = eval_expect_equal({
      q <- wilcox.test(a, b, paired = FALSE, correct = FALSE)
      statistic <- q$statistic
      pValue <- q$p.value
      names(statistic) <- NULL
  },Renv,FLenv,
  verbose = T,
  expectation = c("statistic", "pValue"),
  noexpectation = c("q"),
  tolerance = .03,
  scale = 1
  )
}) 

data1 <- FLTable("tblHypoTest", 
                obs_id_colname="OBSID", 
                whereconditions = "TESTTYPE = 'MWTest' AND GROUPID = 1")
data2 <- FLTable("tblHypoTest", 
                obs_id_colname="OBSID", 
                whereconditions = "TESTTYPE = 'MWTest' AND GROUPID = 2")
FLenv$v1 <- data1$NUM_VAL
FLenv$v2 <- data2$NUM_VAL
Renv$v1 <- as.vector(FLenv$v1)
Renv$v2 <- as.vector(FLenv$v2)


test_that("Test Case for tblHypoTest", {
    result = eval_expect_equal({
        res <- wilcox.test(v1, v2, paired =FALSE)
        statistic <- res$statistic
        pValue <- res$p.value
        names(statistic) <- NULL
    },Renv,FLenv,
    expectation = c("statistic", "pValue"),
    noexpectation = c("res"),
    check.attributes = F
    )
})