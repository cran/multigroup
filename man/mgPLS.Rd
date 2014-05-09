\name{mgPLS}
\alias{mgPLS}
\title{Multigroup Partial Least Squares Regression}
\usage{
  mgPLS(DataX, DataY, Group, ncomp = NULL, Scale = FALSE,
    Gcenter = FALSE, Gscale = FALSE)
}
\arguments{
  \item{DataX}{a numeric matrix or data frame associated
  with independent dataset}

  \item{DataY}{a numeric matrix or data frame associated
  with dependent dataset}

  \item{Group}{a vector of factors associated with group
  structure}

  \item{ncomp}{number of components, if NULL number of
  components is equal to 2}

  \item{Scale}{scaling variables, by defalt is FALSE. By
  default data are centered within groups}

  \item{Gcenter}{global variables centering, by defalt is
  FALSE.}

  \item{Gscale}{global variables scaling, by defalt is
  FALSE.}
}
\value{
  list with the following results:

  \item{DataXm}{Group X data}

  \item{DataYm}{Group Y data}

  \item{Concat.X}{Concatenated X data}

  \item{Concat.Y}{Concatenated Y data}

  \item{coefficients}{Coefficients associated with X data}

  \item{coefficients.Y}{Coefficients associated with
  regressing Y on Global components X}

  \item{Components.Global}{Conctenated Components for X and
  Y}

  \item{Components.Group}{Components associated with groups
  in X and Y}

  \item{loadings.common}{Common vector of loadings for X
  and Y}

  \item{loadings.Group}{Group vector of loadings for X and
  Y}

  \item{expvar}{Explained variance associated with global
  components X}

  \item{cum.expvar.Group}{Cumulative explained varaince in
  groups of X and Y}

  \item{Similarity.Common.Group.load}{Cumulative similarity
  between group and common loadings}

  \item{Similarity.noncum.Common.Group.load}{ NonCumulative
  similarity between group and common loadings}
}
\description{
  Multigroup PLS regression
}
\examples{
data(oliveoil)
DataX = oliveoil[,2:6]
DataY = oliveoil[,7:12]
Group = as.factor(oliveoil[,1])
res.mgPLS = mgPLS (DataX, DataY, Group)
}
\references{
  A. Eslami, E. M. Qannari, A. Kohler and S. Bougeard
  (2013). Multi-group PLS regressMathematics and
  Statistics, Springer Proceedings (ed), \emph{New
  Perspectives in Partial Least Squares and Related
  Methods}, 56, 243-255.

  A. Eslami, E. M. Qannari, A. Kohler and S. Bougeard
  (2014). Algorithms for multi-group PLS. \emph{Journal of
  Chemometrics}, 28(3), 192-201.
}
\seealso{
  \code{\link{mgPCA}}, \code{\link{mbmgPCA}}
}

