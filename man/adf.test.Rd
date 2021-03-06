% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adf.test.R
\name{adf.test}
\alias{adf.test}
\title{Augmented Dickey Fuller Test}
\usage{
adf.test(object, ...)
}
\arguments{
\item{object}{FLVector}

\item{k}{the lag order to calculate the test statistic.}

\item{trend}{1 for including trend.}
}
\value{
A list with class "htest" containing the following components:
\describe{
\item{statistic}{the value of the test statistic.}
\item{parameter}{the lag order.}
\item{p.value}{the p-value of the test.}
\item{method}{a character string indicating what type of test was performed.}
\item{data.name}{a character string giving the name of the data.}
\item{alternative}{a character string describing the alternative hypothesis.}
}
}
\description{
Computes the Augmented Dickey-Fuller test for the null that x has a unit root.
}
\details{
The general regression equation which incorporates a constant and a linear trend
is used and the t-statistic for a first order autoregressive coefficient equals one is computed.
The number of lags used in the regression is k. The default value of trunc((length(x)-1)^(1/3))
corresponds to the suggested upper bound on the rate at which the number of lags, k, should be made to grow with the sample size for the general ARMA(p,q) setup.
}
\examples{
x<-rnorm(1000)
flv<-as.FLVector(x)
flobj<-adf.test(object = flv, k=2)
     y <- diffinv(x)   # contains a unit-root
fly <- as.FL(y)
flobjy <- adf.test(object = fly, k=2)
robj<-adf.test(object = y, k=2)
}
