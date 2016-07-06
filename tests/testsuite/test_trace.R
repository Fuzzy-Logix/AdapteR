
## Testing FLTrace
test_that("check FLTrace",
{
    expect_eval_equal(initF.FLMatrix,
                    AdapteR::tr,
                    psych::tr,
                    n=5,
                    isSquare=TRUE)
})
