
## Internationalization??
test_that("Check for FLCleanStr function",{
         flv  <- as.FL(c("let", "us", "test for non-äöü-printable","characters"))
         resultflvector <- FLCleanStr(flv)
         expect_equal(as.R(flv),as.R(resultflvector))
       })
