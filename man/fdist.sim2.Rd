% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch9-fn.R
\name{fdist.sim2}
\alias{fdist.sim2}
\title{Simulation for the F-distribution}
\usage{
fdist.sim2(sig1, sig2, n1, n2, N = 10000, ng = 300, seed = 9857,
  xp = 1:9, dig = 4)
}
\arguments{
\item{sig1}{Standard deviation of the first population}

\item{sig2}{Standard deviation of the second population}

\item{n1}{Sample size of the first population}

\item{n2}{Sample size of the second population}

\item{N}{Number of iterations, Default: 10000}

\item{ng}{Number of classes in histogram, Default: 300}

\item{seed}{Seed value for generating random numbers, Default: 9857}

\item{xp}{Specific x-values for cumulative probability F(x), Default: 1:9}

\item{dig}{Number of digits below the decimal point, Default: 4}
}
\value{
None.
}
\description{
Simulation for the F-distribution
}
\examples{
fdist.sim2(sig1=2, sig2=7, n1=8, n2=6)
fdist.sim2(n1=5, n2=4)
}
