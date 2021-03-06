% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch14-fn.R
\name{mreg.aov}
\alias{mreg.aov}
\title{Test for the Significance of a Multiple Regression Model}
\usage{
mreg.aov(form, alp = 0.05, dig = 4)
}
\arguments{
\item{form}{Formula of regression model (ex: y ~ x1 + x2)}

\item{alp}{Level of significance, Default: 0.05}

\item{dig}{Number of digits below the decimal point, Default: 4}
}
\value{
ANOVA table.
}
\description{
Test for the Significance of a Multiple Regression Model via ANOVA.
}
\examples{
attach(mtcars)
form = mpg ~ hp + drat + wt
mreg.aov(form)

data(exa14_10)
Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
form2 = Score ~ Study + Reading
mreg.aov(form2)
}
