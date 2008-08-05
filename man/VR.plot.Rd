\name{VR.plot}
\alias{VR.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Variance Ratio Plot }
\description{
  Plotting unstandadized variance ratios against holding periods with 95percent confidence band

  Standard errors under iid returns are used.
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
data(exrates)
y <- exrates$ca                                  # read Canadian exchange rate
nob <- length(y)
r <- log(y[2:nob])-log(y[1:(nob-1)])           # log return calculation
kvec <- c(2,5,10)
VR.plot(r,kvec)
}

\keyword{  htest  }