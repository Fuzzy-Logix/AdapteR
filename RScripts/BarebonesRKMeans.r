# Barebones R KMeans


# a 2-dimensional example
Data <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
res <- kmeans(x, 5, iter.max=30, nstart = 25)