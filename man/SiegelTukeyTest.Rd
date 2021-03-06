% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SiegelTukeyTest.R
\name{SiegelTukeyTest}
\alias{SiegelTukeyTest}
\title{Siegel-Tukey Test For Equality In Variability.}
\usage{
SiegelTukeyTest(x, y, alternative = "two.sided", conf.level = 0.95, ...)
}
\arguments{
\item{x:}{FLVector of data values}

\item{y:}{FLVector of data values}
}
\value{
A list with class "htest".
}
\description{
Non-parametric Siegel-Tukey test for equality in variability. The null 
hypothesis is that the variability of x is equal between two groups. A 
rejection of the null hypothesis indicates that variability differs 
between the two groups. SiegelTukeyRank returns the ranks,
calculated after Siegel Tukey logic.
}
\examples{
x <- as.FLVector(rnorm(50))
y <- as.FLVector(rnorm(30))
SiegelTukeyTest(x, y)
}
\seealso{
\code{\link[DescTools]{SiegelTukeyTest}} for R reference implementation.
}
