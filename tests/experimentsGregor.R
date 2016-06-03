## here goes all stuff that I am working on and that is
## not yet ready for inclusion in the test suite
data(iris)


km <- kmeans(iris[,1:4],3)
km$centers
