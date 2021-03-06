% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch13-fn.R
\name{anova1.fit}
\alias{anova1.fit}
\title{One-Way Analysis of Variance}
\usage{
anova1.fit(y, f, xl, yl, alp = 0.05, bplot = TRUE, plot = TRUE, dig = 4)
}
\arguments{
\item{y}{Response variable data.}

\item{f}{Factors (same length as y).}

\item{xl}{Label of x-axis, Default: 'Factor'.}

\item{yl}{Label of y-axis, Default: 'Response Variable'.}

\item{alp}{Level of significance, Default: 0.05.}

\item{bplot}{Logical value for drawing a box plot for prior investigation (default=TRUE).}

\item{plot}{Logical value for drawing diagnostic plots (default=TRUE).}

\item{dig}{Number of digits below the decimal point, Default: 4.}
}
\value{
None.
}
\description{
One-way analysis of variance to produce an ANOVA table.
}
\examples{
y = c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
f = c(rep(150,5), rep(200,6), rep(250,5), rep(300,4))
anova1.fit(y, f, xl="Temp", yl="Yield")
}
