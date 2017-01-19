
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
  verbose = FALSE
  )
  }) 

