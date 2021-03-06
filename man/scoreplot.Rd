% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoreplot.R
\name{scoreplot}
\alias{scoreplot}
\title{Score plot for multigroup data}
\usage{
scoreplot(x, axes = c(1, 2), cex = NULL, font.lab = NULL)
}
\arguments{
\item{x}{results of the proposed multigroup methods in the package}

\item{axes}{a vector of two selected components}

\item{cex}{character expansion for text by default .85}

\item{font.lab}{type of font by default 3}
}
\value{
score plot
}
\description{
plots of individuals
}
\examples{
Data = iris[,-5]
Group = iris[,5]
res.mgPCA = mgPCA (Data, Group, graph=TRUE)
scoreplot(res.mgPCA, axes=c(1,2))
}
