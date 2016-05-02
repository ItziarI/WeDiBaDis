\name{dBhatta}
\alias{dBhatta}
\title{ Bhattacharyya distance}
\description{ This function calculates the Bhattacharyya distance between genetic populations.}
\usage{
dBhatta(x)
}
\arguments{
  \item{x}{ a data matrix with the frequencies where the sum of each row (population) must be equal to 1.}
  }
  \details{ \code{dBhatta} allows to include missing values (such as NA). When a missing  value is present for an individual, the corresponding variable is excluded in calculating the distances for that individual.
}
\value{returns an object of class "\code{dist}".
}
\references{
Bhattacharyya, A. (1946). \emph{On a Measure of Divergence of Two Multinominal Populations}, Sankhya, \bold{7}, 401--406.
}

\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}
\keyword{multivariate}
\seealso{\code{\link{dist}}, \code{\link{dBrayCurtis}}, \code{\link{dcor}}, \code{\link{dGower}}, \code{\link{dHellinger}}, \code{\link{dMahal}}, \code{\link{dOrloci}}, \code{\link{dPrevosti}} }
\examples{## Frequency Matrix of Multivariate sample without Missing Values:
sites <- 50
n <- 100
x <- abs(matrix(rnorm(n*sites), ncol = sites))
x <- t(apply(x, 1, function(a){a/sum(a)}))
dBhatta(x)

## Frequency Matrix of Multivariate sample and Missing Values:
sites <- 50
n <- 100
x <- abs(matrix(rnorm(n*sites), ncol = sites))
x <- t(apply(x, 1, function(a){a/sum(a)}))
x[1, 1] <- NA
dBhatta(x)
}
