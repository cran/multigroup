\name{mgPCA}
\alias{mgPCA}
\title{Multi-group Principal Component Analysis}
\usage{
mgPCA(Data, Group, ncomp = NULL, Scale = FALSE, graph = FALSE)
}
\arguments{
  \item{Data}{a numeric matrix or data frame}

  \item{Group}{a vector of factors associated with group
  structure}

  \item{ncomp}{number of components, if NULL number of
  components is equal to 2}

  \item{Scale}{scaling variables, by defalt is False. By
  default data are centered within groups.}

  \item{graph}{should loading and component be plotted}
}
\value{
list with the following results:

\item{Data}{original data}

\item{Con.Data}{Concatenated centered data}

\item{split.Data}{Group centered data}

\item{Group}{Group as a factor vector}

\item{loadings.group}{Loadings associated with each group}

\item{score.group}{Scores associated with each group}

\item{loadings.common}{Matrix of common loadings}

\item{score.Global}{Global scores}

\item{cumper.inertigroup}{Cumulative percentage of group
components inertia}

\item{cumper.inertiglobal}{Cumulative percentage of global
component inertia}

\item{noncumper.inertiglobal}{Percentage of global
component inertia}

\item{lambda}{The specific variances of group}

\item{exp.var}{Percentages of total variance recovered
associated with each dimension }
}
\description{
Multi-group PCA algorithm (NIPALS for Multi-group PCA)
}
\examples{
Data = iris[,-5]
Group = iris[,5]
res.mgPCA = mgPCA (Data, Group, graph=TRUE)
loadingsplot(res.mgPCA, axes=c(1,2))
scoreplot(res.mgPCA, axes=c(1,2))
}
\references{
A. Eslami, E. M. Qannari, A. Kohler and S. Bougeard (2013).
General overview of methods of analysis of multi-group
datasets, \emph{Revue des Nouvelles Technologies de
l'Information}, 25, 108-123.
}
\seealso{
\code{\link{BGC}}, \code{\link{FCPCA}},
\code{\link{DCCSWA}}, \code{\link{DSTATIS}},
\code{\link{DGPA}}, \code{\link{summarize}},
\code{\link{TBWvariance}}, \code{\link{loadingsplot}},
\code{\link{scoreplot}}, \code{\link{iris}}
}

