\name{corearea}
\alias{corearea}
\docType{data}
\title{
Core area data
}
\description{
Ecological data about the core area (the region close
to the nest) of the golden eagle \emph{Aquila chrysaetos} in three regions of Western Scotland.
}
\usage{corearea}
\format{
  A data frame with 40 observations on the following 8 variables:
  \describe{
    \item{\code{Class}}{region}
    \item{\code{POST}}{mature planted conifer forest in which the tree canopy has closed}
    \item{\code{PRE}}{pre-canopy closure planted conifer forest}
    \item{\code{BOG}}{at waterlogged land}
    \item{\code{CALL}}{Calluna (heather) heath land}
    \item{\code{WET}}{wet heath, mainly purple moor grass}
    \item{\code{STEEP}}{steeply sloping land}
    \item{\code{LT200}}{land below 200 m}
    \item{\code{L4600}}{land between 200 and 400 m}
  }
}
\details{
The values are the numbers of four-hectare grid cells covered by the habitat, whose values are the amounts of each habitat variable, measured as the number of four hectare blocks within a region defined as a "core area".
}
\source{
Fielding A. (2007) \emph{Cluster and Classification Techniques for the Biosciences}. Cambridge University Press.
}

\examples{
data("corearea")
}
\keyword{datasets}
