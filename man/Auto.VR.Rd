\name{Auto.VR}
\alias{Auto.VR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Automatic Variance Ratio Test }
\description{
  A variance ratio test with holding period value chosen by a data dependent procedure
}
\usage{
Auto.VR(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ financial return time series }
}

\value{
 
 
  \item{AutoVR }{Automatic variance ratio test statistic}
 
}
\references{ Choi, I. 1999, Testing the random walk hypothesis for real exchange rates
              Journal of Applied Econometrics, 14, 293-308.}
\author{ Jae H. Kim}
\note{ R code translated from Choi's GAUSS code
}
\examples{
y <- rnorm(100)
Auto.VR(y)}
\keyword{  htest  }

