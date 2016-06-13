data(iris)

##test_that("iris table upload works",{
irisFL <- as.FLTable(iris)

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



## asana
test_that("iris table upload works -- names, use mapping table",{
    expect_equal(
        names(iris),
        names(irisFL),
        check.attributes=FALSE)
})



## asana
test_that("FLTable in-database transformations work -- ALTER TABLE and UPDATE",{
    irisFL$SepalArea <-            irisFL$SepalLength * irisFL$SepalWidth
    expect_equal(irisFL$SepalArea,
                 iris$Sepal.Length  * iris$Sepal.Width)

    irisFL$SepalLongerThanWide2 <- irisFL$SepalLength > irisFL$SepalWidth*2
    expect_equal(irisFL$SepalLongerThanWide2,
                 iris$Sepal.Length   > iris$Sepal.Width*2)

    irisFL$SepalBoxLength <- 2 * (irisFL$SepalLength + irisFL$SepalWidth)
    expect_equal(irisFL$SepalBoxLength,
                 2 * (iris$Sepal.Length   + iris$Sepal.Width))
})


test_that("Selection of columns works with $ and with [,name]",{
    ## A remote matrix is easily created by specifying
    ## table, row id, column id and value columns
    DfilmF <- FLTable(database          = "FL_DEMO",
                      table        = "actressldist",
                      obs_id_colname    = "ObsID")
    expect_equal(as.vector(head(DfilmF$Actor)),
                 as.vector(head(DfilmF[,"Actor"])))
})
