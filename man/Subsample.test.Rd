\name{Subsample.test}
\alias{Subsample.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Subsampling test of Whang and Kim (2003) }
\description{
  The function returns the p-values of the subsampling test.
}
\usage{
Subsample.test(y, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector of time series, typically  financial return }
  \item{kvec}{ a vector of holding periods }
}
\details{
 The block lengths are chosen internally using the rule proposed in Whang and Kim (2003)
}
\value{
  
  \item{Holding.Period}{holding periods used}
  \item{Block.Length}{block lengths chosen}
  \item{pval}{p-values of the test for each block length used}
}
\references{ WHANG,Y.-J., J. KIM, 2003, A Multiple Variance Ratio Test Using Subsampling, Economics Letters, 79, 225-230.

}
\author{ Jae H. Kim }

\examples{
y <- rnorm(100)
kvec <- c(2,5,10)
Subsample.test(y,kvec)
}

\keyword{  htest  }