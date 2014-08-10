\name{Panel.VR}
\alias{Panel.VR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Panel Variance Ratio Tests
}
\description{
Panel variance tatio tests based on Maximum Absloute Value, Sum of Squares, and Mean of each cross-sectional units
}
\usage{
Panel.VR(dat, nboot = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
a T by K matrix of asset returns, K is the munber of cross sectional units and T is length of time series
}
  \item{nboot}{
the number of wild bootstrap iterations, the default is set to 500
}
}
\details{
The component statistics are based on the automatic variance ratio test
The set of returns are wild bootstrapped to conserve cross-sectional dependency

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{MaxAbs.stat }{the statistic based on the maximum absolute value of individual statistics}
 \item{SumSquare.stat }{the statistic based on the sum of squared value of individual statistics}
 \item{Mean.stat }{the statistic based on the mean value of individual statistics}
 \item{MaxAbs.pval }{the wild bootstrap pvalue based on the maximum absolute value of individual statistics}
 \item{SumSquare.pval }{the wild bootstrap pvalue based on the sum of squared value of individual statistics}
 \item{Mean.pval }{the wild bootstrap pvalue based on the mean value of individual statistics}

%% ...
}
\references{
Kim, Jae H. and Shamsuddin, Abul, A Closer Look at Return Predictability of the US Stock Market: Evidence from a Panel Variance Ratio Test (February 13, 2013). 
Available at SSRN: http://ssrn.com/abstract=2217248 or http://dx.doi.org/10.2139/ssrn.2217248 
}
\author{
Jae H. Kim
}

\examples{
ret=matrix(rnorm(200),nrow=100)
Panel.VR(ret)
}

\keyword{  htest  }