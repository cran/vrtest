\name{JWright.crit}
\alias{JWright.crit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Critical Values for the joint versions of Wright's rank and sign tests  }
\description{
  This function runs a simulation to calculate the critical values of the joint versions of Wright's tests.
}
\usage{
JWright.crit(n, kvec, nit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ sample size }
  \item{kvec}{ holding period vector }
  \item{nit}{ number of iterations }
}

\value{
  \item{Holding.Period }{holding period used}
  \item{JR1.crit }{Critical values for the joint R1 statistic}
   \item{JR2.crit }{Critical values for the joint R2 statistic}
   \item{JS1.crit }{Critical values for the joint S1 statistic}
}
\references{

Belaire-Franch G, Contreras D. Ranks and signs-based multiple variance ratio tests, Working paper, University of Valencia 2004. 

Kim, J. H. and Shamsuddin, A., Are Asian Stock Markets Efficient? Evidence from New Multiple Variance Ratio Tests, Working Paper, Monash University.}
\author{ Jae H. Kim }

\examples{
y <- as.matrix(rnorm(100))
kvec <- c(2,5,10)
JWright.crit(n=nrow(y),kvec,nit=10000)  
}
\keyword{  htest  }