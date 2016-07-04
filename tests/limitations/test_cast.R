test_that("Internationalization: Upload of German umlauts",{
    rv <- c("let", "us", "test for non-äöü-printable","characters")
    flv  <- as.FL(rv)
    expect_equal(as.R(flv),rv)
})
