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
digitalPCR(pos, n, dilution, Nboot, single.copy = c("FALSE", "TRUE"))
}

\arguments{
  \item{pos}{
 a numeric vector of number of positive reactions in the order of dilutions
}
  \item{n}{
 a number or a numeric vector of number of total reactions in the order of dilutions
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
}
\details{
The total number n can be a vector or a number which will be recycled. If the assay is assumed single-copy sensitive, single.copy can be set to "TRUE". If the assay contains 3 dilutions at 1, 2, and 4 fold relative to initial concentrate, set dilution=c(1,2,4)
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
n=384
#note in practice, set this 100 or larger
Nboot=10 

result1=digitalPCR(pos, n, dilution,  Nboot)
print (paste(result1$"mean copy number", result1$"sd copy number"))
hist(result1$"thresholds",xlim=c(-2,10))

#the following example contains only 1 dilution
result2=digitalPCR(230, 355, 1,  Nboot, "TRUE")
}
