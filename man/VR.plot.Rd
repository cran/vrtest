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

\author{ Jae H. Kim & Alexios Ghalanos}


\examples{
data(exrates)
y <- exrates$ca                                 
nob <- length(y)
r <- log(y[2:nob])-log(y[1:(nob-1)])
kvec <- c(2,5,10)
VR.plot(r,kvec)
}

\keyword{  htest  }