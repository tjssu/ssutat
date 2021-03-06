% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch4-fn.R
\name{disc.marg2}
\alias{disc.marg2}
\title{Marginal PDF of Two Discrete Random variables}
\usage{
disc.marg2(tabXY, Xn, Yn, prt = TRUE, plot = FALSE, dig = 5, dig2 = 4)
}
\arguments{
\item{tabXY}{Joint frequency table of two random variables}

\item{Xn}{Name of the first random variable (default="X")}

\item{Yn}{Name of the second random variable (default="Y")}

\item{prt}{Print the marginal frequency and probability? Default: TRUE}

\item{plot}{Plot the marginal PDF? Default: FALSE}

\item{dig}{Number of digits below the decimal point in the console, Default: 5}

\item{dig2}{Number of digits below the decimal point in the graph, Default: 4}
}
\value{
Marginal Probabilities
}
\description{
Marginal PDF of Two Discrete Random variables
}
\examples{
fxy = with(mtcars, table(cyl, carb))
disc.marg2(fxy)
disc.marg2(fxy, "Cylinder", "Carbrator", prt=FALSE, plot=TRUE)

for (x in 0:4) for (y in 0:3) fxy[x+1, y+1] = x+y^2
rownames(fxy) = 0:4; colnames(fxy) = 0:3
disc.marg2(fxy)
disc.marg2(fxy, "X", "Y", prt=FALSE, plot=TRUE)
}
