\name{Joint.Wright}
\alias{Joint.Wright}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A Joint Version of Wight's Rank and Sign Test }
\description{
  This function returns joint or multiple version of Wright's rank and sign tests.
  The test takes the maximum value of the individual rank or sign tests, in the same manner as Chow-Denning test
}
\usage{
Joint.Wright(y, kvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector of time series, typically  financial return }
  \item{kvec}{ a vector of holding periods }
}
\details{
  }
\value{
  \item{Holding.Period }{holding periods used}
  \item{JR1 }{Joint test based on R1 statistics}
  \item{JR2 }{Joint test based on R2 statistics}
  \item{JS1 }{Joint test based on S1 statistics}
}
\references{ 
Belaire-Franch G, Contreras D. Ranks and signs-based multiple variance ratio tests, Working paper, University of Valencia 2004. 

Kim, J. H. and Shamsuddin, A., Are Asian Stock Markets Efficient? Evidence from New Multiple Variance Ratio Tests, Working Paper, Monash University.
}
\author{Jae H. Kim}
\examples{
y <- as.matrix(rnorm(100))
kvec <- c(2,5,10)
Joint.Wright(y,kvec) 
}
\keyword{  htest  }