\name{dHellinger}
\alias{dHellinger}
\title{ Hellinger Distance}
\description{ This function calculates Hellinger distance that is recommended for species abundance data sets.}
\usage{
dHellinger(x)
}
\arguments{
  \item{x}{ a data matrix with species abundance.}
  }
  
\details{ The Hellinger distance between two sites y1, y2  and across m species takes into account total the abundances of each site (see Rao (1995))


  \code{dHellinger} allows to include missing values (such as NA). When a missing  value is present for an individual, the corresponding variable is excluded in calculating the distances for that individual. As in \code{\link{dist}}, when some columns are excluded the sum is scaled up proportionally to the number of columns used.
}
\value{It returns an object of class "\code{dist}".
}
\references{
Rao, C. (1995). \emph{A review of canonical coordinates and an alternative to correspondence analysis using Hellinger distance}, Questiio, \bold{19}, 23-65.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}
\keyword{multivariate}

\seealso{\code{\link{dist}}, \code{\link{dBhatta}}, \code{\link{dBrayCurtis}}, \code{\link{dcor}}, \code{\link{dGower}}, \code{\link{dMahal}}, \code{\link{dOrloci}}, \code{\link{dPrevosti}} }

\examples{
## Simulate Abundance Matrix of Multivariate sample without Missing Values:
sites <- 50
m <- 25
x <- matrix(0, sites, m)
for (s in 1:sites)
{
        x[s, ] <- tabulate(sample(1:m, round(runif(1, 100, 200)), replace=TRUE))
}
dHellinger(x)

## Simulate Abundance Matrix of Multivariate sample and Missing Values:
x[1, 1] <- NA
dHellinger(x)
}
