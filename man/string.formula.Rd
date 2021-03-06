% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string.formula.R
\name{string.formula}
\alias{string.formula}
\title{Model in the formula object converted to a character}
\usage{
string.formula(formula)
}
\arguments{
\item{formula}{formula object}
}
\value{
character. model
}
\description{
Model in the formula object converted to a character
}
\examples{
aformula <- as.formula(medv ~ lstat)
astring <- string.formula(aformula)
print(astring)

}
