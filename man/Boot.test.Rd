\name{Boot.test}
\alias{Boot.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bootstrap Variance Ratio Tests }
\description{
  This function returns bootstrap p-values of the Lo-MacKilay (1988) and Chow-Denning (1993) tests.

  Users can choose between iid bootstrap and wild bootstrap
}
\usage{
Boot.test(y, kvec, nboot, wild)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector of time series, typically  financial return}
  \item{kvec}{ a vector of holding periods }
  \item{nboot}{ the number of bootstrap iterations }
  \item{wild}{ "No" for iid bootstrap, "Normal" for the wild bootstrap using the standard normal distribution, "Mammen" for the wild bootstrap using Mammen's two point distribution,
               "Rademacher" for the wild bootstrap using Rademacher's two point distribution}
}

\value{
  
  \item{Holding.Period }{holding periods used}
  \item{LM.pval }{Bootstrap p-values for the Lo-MacKinlay tests}
  \item{CD.pval }{Bootstrap p-value for the Chow-Denning test}
}
\references{ Kim, J.H., 2006, Wild Bootstrapping Variance Ratio Tests. Economics Letters, 92, 38-43. }
\author{ Jae H. Kim }


\examples{
y <- rnorm(100)
kvec <- c(2,5,10)
Boot.test(y,kvec,nboot=1000,wild="Normal")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  htest  }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
