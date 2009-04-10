\name{AutoBoot.test}
\alias{AutoBoot.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Wild Bootstrapping of Automatic Variance Ratio Test }
\description{
  This function returns wild bootstrap test results for the Automatic Variance Ratio Test of Choi (1999)
}
\usage{
AutoBoot.test(y, nboot, wild)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ a vector of time series, typically  financial return}
  \item{nboot}{ the number of bootstrap iterations }
  \item{wild}{ "Normal" for the wild bootstrap using the standard normal distribution, "Mammen" for the wild bootstrap using Mammen's two point distribution,
               "Rademacher" for the wild bootstrap using Rademacher's two point distribution}
}

\value{
  
  \item{test.stat }{Automatic variance ratio test statistic}
  \item{pval }{Wild Bootstrap p-value for the test}
  \item{Quantiles }{Quantiles from Bootstrap distribution}
}
\references{ Kim, J.H., 2009, Automatic Variance Ratio Test under Conditional Heteroskedasticity. Finance Research Letters. Forthecoming }
\author{ Jae H. Kim }


\examples{                                
r <- rnorm(100)           # log return
AutoBoot.test(r,nboot=500,wild="Normal")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  htest  }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
