
test_that("iris matrix upload works -- names, use mapping table",
{
    irism <- as.matrix(iris[,1:4])
    irismFL <- as.FLMatrix(irism)
    ## asana
    ## optimize:
    ## should not have QUERY SQL:
    ## select top 1 * from Fl_demo.tblMatrixNameMapping_test AS flt
    expect_equal(
        colnames(irismFL),
        colnames(irism),
        check.attributes=FALSE)
})





