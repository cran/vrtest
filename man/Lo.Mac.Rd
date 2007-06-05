\name{Lo.Mac}
\alias{Lo.Mac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lo-MacKinlay variance Ratio Tests }
\description{
  The function returns M1 and M2 statistics of Lo and MacKinlay (1998).

  M1 tests for iid series and M2 for uncorrelated series with possible heteroskedasticity.
}
\usage{
Lo.Mac(y, kvec)
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
  \item{M1.stat }{M1 test statistic}
  \item{M2.stat }{M2 test statistic}
 
}
\references{ LO, A. W., and A. C. MACKINLAY (1988): "Stock Market Prices Do Not Follow Random Walks: Evidence from a Simple Specification Test," The Review of Financial Studies, 1, 41-66. }
\author{ Jae H. Kim}

\examples{
y <- as.matrix(rnorm(100))
kvec <- c(2,5,10)
Lo.Mac(y,kvec) 
}

\keyword{ htest   }