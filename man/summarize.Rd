\name{summarize}
\alias{summarize}
\title{Summary}
\usage{
  summarize(Data, Group)
}
\arguments{
  \item{Data}{a numeric matrix or data frame}

  \item{Group}{a vector of factors associated with group
  structure}
}
\value{
  list with the following results:

  \item{Global.summary}{ summary of globala data}

  \item{Group.summary}{ summary of group datasets}

  \item{mean.between.data}{ matrix of Group mean}

  \item{mean.within.data}{ matrix of group centered data}
}
\description{
  Summary of multi-group data in global and group parts
}
\examples{
Data = iris[,-5]
Group = iris[,5]
res = summarize(Data, Group)
}
\seealso{
  \code{\link{mgPCA}}, \code{\link{DGPA}},
  \code{\link{DCCSWA}}, \code{\link{DSTATIS}},
  \code{\link{BGC}}, \code{\link{TBWvariance}},
  \code{\link{iris}}
}

