% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/small_functions.R
\name{fbind}
\alias{fbind}
\title{Bind two factors Wurds}
\usage{
fbind(a, b)
}
\arguments{
\item{a}{factor}

\item{b}{factor}
}
\value{
factor
}
\description{
Create a new single factor from two other factors without dumbly converting
them to integers
}
\examples{
fbind(iris$Species[c(1,51,101)], PlantGrowth$group[c(1,11,21)])

}
