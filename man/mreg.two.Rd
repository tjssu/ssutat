% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch14-fn.R
\name{mreg.two}
\alias{mreg.two}
\title{Compare Two Multiple Regression Models}
\usage{
mreg.two(form1, form2, plot = FALSE, detail = FALSE, wh = c(1:3, 5),
  alp = 0.05, dig = 4)
}
\arguments{
\item{form2}{Formula for another regression model (eg: y ~ x1 * x2)}

\item{plot}{Logical value for drawing diagnosis plots, Default: FALSE.}

\item{detail}{Logical value for printing detailed calculations, Default: FALSE.}

\item{wh}{Index vector for the diagnosis plots, Default: c(1:3,5).}

\item{alp}{Level of significance, Default: 0.05}

\item{dig}{Number of digits below the decimal point, Default: 4}

\item{form}{Formula of regression model (ex: y ~ x1 + x2)}
}
\value{
None.
}
\description{
Compare Two Multiple Regression Models.
}
\examples{
attach(mtcars)
form.1 = mpg ~ hp + drat + wt
form.2 = mpg ~ hp * drat * wt
mreg.two(form.1, form.2)
mreg.two(lm(form.1), lm(form.2))

data(exa14_10)
Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
form2.1 = Score ~ Study + Reading
form2.2 = Score ~ Study * Reading
mreg.two(form2.1, form2.2, plot=T)
mreg.two(lm(form2.1), lm(form2.2), plot=T)
}
