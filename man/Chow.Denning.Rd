\name{Chow.Denning}
\alias{Chow.Denning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Chow-Denning Multiple Variance Ratio Tests }
\description{
  This function returns Chow-Denning test statistics.

  CD1 tests for iid series and CD2 for uncorrelated series with possible heteroskedasticity.
}
\usage{
Chow.Denning(y, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector of time series, typically  financial return }
  \item{kvec}{ a vector of holding periods }
}
\details{
  
}

\value{
  
  \item{Holding.Periods }{holding periods used}
  \item{CD1 }{CD1 statistic}
  \item{CD2 }{CD2 statistic}
  \item{Critical.Values_10_5_1_percent }{10 5 1 percent critical values}
}

\references{ Chow,K. V., K. C. DENNING, 1993, A Simple Multiple Variance Ratio Test, Journal of Econometrics, 58, 385-401. }
\author{ Jae H. Kim }


\examples{
y <- as.matrix(rnorm(100))
kvec <- c(2,5,10)
Chow.Denning(y,kvec) 
}

\keyword{  htest  }