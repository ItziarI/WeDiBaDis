\name{dOrloci}
\alias{dOrloci}
\title{ Orloci Distance}
\description{ This function calculates Orloci distance that is recommended for species abundance data sets.}
\usage{
dOrloci(x)
}
\arguments{
  \item{x}{ a data matrix with species abundance.}
  }
  
\details{ The Orloci distance between two sites y1, y2  and across m species takes into account the total abundances of each site (see Orloci (1967))


  \code{dOrloci} allows to include missing values (such as NA). When a missing  value is present for an individual, the corresponding variable is excluded in calculating the distances for that individual. As in \code{\link{dist}}, when some columns are excluded the sum is scaled up proportionally to the number of columns used.
}
\value{It returns an object of class "\code{dist}".
}
\references{
Orloci, L. (1967). An agglomerative method for classification of plant communities, \emph{Journal of Ecology}, \bold{55}, 193--205.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}
\keyword{multivariate}

\seealso{\code{\link{dist}}, \code{\link{dBhatta}}, \code{\link{dBrayCurtis}}, \code{\link{dcor}}, \code{\link{dGower}}, \code{\link{dHellinger}}, \code{\link{dMahal}},  \code{\link{dPrevosti}} }

\examples{
## Simulate Abundance Matrix of Multivariate sample without Missing Values:
sites <- 50
m <- 25
x <- matrix(0, sites, m)
for (s in 1:sites)
{
        x[s, ] <- tabulate(sample(1:m, round(runif(1, 100, 200)), replace=TRUE))
}
dOrloci(x)

## Simulate Abundance Matrix of Multivariate sample and Missing Values:
x[1, 1] <- NA
dOrloci(x)
}
