\name{DL.test}
\alias{DL.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dominguez-Lobato Test for Martingale Difference Hypothesis }
\description{
  Dominguez-Lobato Test
}
\usage{
DL.test(y,B,p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ financial return time series }
  \item{B}{ the number of bootstrap iterations, the default is 300}
  \item{p}{ the lag value, the default is 1}
}

\value{
  \item{Cp}{Cramer von Mises test statistic}
  \item{Kp}{Kolmogorov-Smirnov test statistic}
  \item{Cp_pval}{wild bootstrap p-value of the Cp test}
  \item{Kp_pval}{wild bootstrap p-value of the Kp test}
}
\references{ 
Domingues M.A. and Lobato, I. N., 2003,  Testing the Martingale Difference Hypothesis, Econometrics Reviews, 22, p351-377.

Charles, A. Darne, O. Kim, J.H. 2011, Small Sample Proeprties of Alternative Tests for Martingale Difference Hypothesis, Economics Letters, in press.
}
\author{ Jae H. Kim}

\examples{
r <- rnorm(100)           # log return calculation
DL.test(r)
}
\keyword{  htest  }
