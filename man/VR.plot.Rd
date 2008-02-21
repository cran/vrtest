\name{VR.plot}
\alias{VR.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Variance Ratio Plot }
\description{
  Plotting unstandadized variance ratios against holding periods
}
\usage{
VR.plot(y, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ financial return}
  \item{kvec}{ holding period vector }
}
\value{ 
  
  \item{VR }{ vector of variance ratio values plotted}
 
}

\author{ Jae H. Kim }


\examples{
y <- rnorm(100)
kvec <- c(2,5,10)
VR.plot(y,kvec)
}

\keyword{  htest  }