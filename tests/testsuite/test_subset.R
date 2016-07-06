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
