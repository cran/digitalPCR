\name{digitalPCR}
\alias{digitalPCR}
\title{Estimate copy number and assay sensitivity from dPCR serial dilution}
\description{
The assay sensitivity is the minimum number of copies that the digital 
PCR assay can detect. Users provide serial dilution results in the format of counts 
of positive and total reaction wells. The output is the estimated assay sensitivity 
and the copy number per well in the initial dilute.
}
\usage{
digitalPCR(pos, neg, dilution, Nboot, single.copy = c("FALSE", "TRUE"), upper.copy=100)
}

\arguments{
  \item{pos}{
 a numeric vector of number of positive reactions in the order of dilutions
}
  \item{neg}{
 a numeric vector of number of negative reactions in the order of dilutions
}
  \item{dilution}{
 a numeric vector of folds of dilutions relative to the initial dilution
}
  \item{Nboot}{
 number of bootstrapping
}
  \item{single.copy}{
 character tells whether the assay is assumed single-copy sensitive. default "FALSE"
}
  \item{upper.copy}{
 the upper bound of copy number in highest concentration
}
}
\details{
the pos, neg and dilution vector must be in the same order from high to low concentrations.
}
\value{
The returned value is a list of four components:
\item{mean copy number }{mean copy number}
\item{sd copy number}{standard deviation of copy number}
\item{copy numbers }{bootstrapped copy number estimates}
\item{thresholds}{bootstrapped assay sensitivity estimates}
}

\author{ Xutao Deng }


\examples{
#dilution at 1,2,4,8 fold
pos=c(221,97,39,14)
dilution=c(1,2,4,8)
neg=384-pos
#note in practice, set this 100 or larger
Nboot=10 

result1=digitalPCR(pos, neg, dilution,  Nboot)
print (paste(result1$"mean copy number", result1$"sd copy number"))
hist(result1$"thresholds",xlim=c(-2,10))

#the following example contains only 1 dilution
result2=digitalPCR(230, 355, 1,  Nboot, "TRUE")
}