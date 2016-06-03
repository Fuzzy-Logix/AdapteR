data(iris)

##test_that("iris table upload works",{
irisFL <- as.FLTable(iris)




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
