\name{Wald}
\alias{Wald}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Wald Test of Richardson and Smith (1991) }
\description{
  This function returns the Wald test statistic with critical values
}
\usage{
Wald(y, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector of time series, typically  financial return }
  \item{kvec}{ a vector of holding periods }

  
}
\value{
  
  \item{Holding.Periods}{holding periods used}
  \item{Wald.stat}{Wald test statistic}
  \item{Critical.Values_10_5_1_percent}{10 5 and 1 percent critical values}
}
\references{ Richardson, M., T. Smith, 1991, "Tests of Financial Models in the Presence of Overlapping Observations," The Review Financial Studies, 4, 227-254. }
\author{ Jae H. Kim }
\note{ The statistic asymptotically follows the chi-squared distribution with the degrees of freedom same as the number of holding periods used}
\examples{
y <- as.matrix(rnorm(100))
kvec <- c(2,5,10)
Wald(y,kvec) 
}
\keyword{  htest  }