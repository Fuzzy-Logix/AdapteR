# kuiper Test.
test_that("Kuiper Test -- DBLytix Example ",{
    str <- paste0("SELECT * FROM tblKuiperTest")
    res1 <- sqlQuery(connection, str)
    alpha <- as.FL(res1[res1$groupid == 1, ]$num_val)
    beta <- as.FL(res1[res1$groupid == 2, ]$num_val)
    fit <- kuip.test(alpha, beta)
    expect_equal(fit,
                 ##dput(fit)
                 structure(list(statistics = structure(0.386363636363636, .Names = "Stat"), 
                                p.value = 0.276808688068662,
                                method = "2-Sample Kuiper Test", 
                                data.name = "alpha and beta"),
                           .Names = c("statistics", "p.value", 
                                      "method", "data.name"), class = "htest"))
})


