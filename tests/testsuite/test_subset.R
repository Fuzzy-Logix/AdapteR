## Testing FLSubsetting
test_that("check vector subsetting",
{
  ## Testing result
  expect_eval_equal(initF.FLVector,
                    function(x) do.call("[",list(x,5:3)),
                    function(x) do.call("[",list(x,5:3)),n=5)
  expect_eval_equal(initF.FLVector,
                    function(x) do.call("[",list(x)),
                    function(x) do.call("[",list(x)),n=5)  
})

test_that("Reccursively check FLSimpleVector subsetting:",
{
  ## Testing result
  flSimple <- FLSerial(1,10)
    ##names(flSimple) <- letters[10:1] ## todo: add names support
  expect_equal_FLSimpleVector_RVector(flSimple)
})
