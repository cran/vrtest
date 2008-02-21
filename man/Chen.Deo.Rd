\name{Chen.Deo}
\alias{Chen.Deo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Power Transformed Joint Variance Ratio Test }
\description{
  See equation (15) of Chen and Deo (2006)
}
\usage{
Chen.Deo(x, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of time series, typically  financial return }
  \item{kvec}{ a vector of holding periods}
}

\value{
  \item{Holding.Period }{holding periods used}
  \item{VRsum }{ the sum of (power transformed individual VR - 1)}
  \item{QPn }{QPn statistic}
}
\references{ Chen, W. W., and Deo, R.S., 2006, The Variance Ratio Statistic at Large Horizons, Econometric Theory, 22, 206-234.}
\author{ Jae H. Kim }

\examples{
y <- rnorm(100)
kvec <- c(2,5,10)
Chen.Deo(y,kvec)
}
\keyword{  htest  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
