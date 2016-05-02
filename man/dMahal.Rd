\name{dMahal}
\alias{dMahal}
\title{ Mahalanobis Distance}
\description{ This function calculates the Mahalanobis distance.}
\usage{
dMahal(x, tol=1e-8)
}
\arguments{
  \item{x}{ a data matrix.}
  \item{tol}{ considered tolerance when verifying the var-cov matrix is not singular.}
  }
\details{ Usual Mahalanobis distance between two individuals with S the variance-covariance matrix estimated in \code{x}. 

  \code{dMahal} allows to include missing values (such as NA). When a missing  value is present for an individual, the corresponding variable is excluded in calculating the distances for that individual. As in \code{\link{dist}}, when some columns are excluded the sum is scaled up proportionally to the number of columns used.
}
\value{ It returns an object of class "\code{dist}".
}
\references{
Mahalanobis, P. (1936). On the generalized distance in statistics, \emph{Procedures of the Natural Institute of India}, \bold{2}, 49-55.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}

\keyword{multivariate}

\seealso{\code{\link{dist}}, \code{\link{dBhatta}}, \code{\link{dBrayCurtis}}, \code{\link{dcor}}, \code{\link{dGower}},  \code{\link{dHellinger}}, \code{\link{dOrloci}}, \code{\link{dPrevosti}} }

\examples{
## Matrix of Multivariate sample without Missing Values:
n.var<-30
n.samples<-100
x <- matrix(rnorm(n.samples*n.var), ncol = n.var)
dMahal(x)

## Matrix of Multivariate sample and Missing Values:
n.var<-30
n.samples<-100
x <- matrix(rnorm(n.samples*n.var), ncol = n.var)
x[1,1] <- NA
dMahal(x)
}
