\name{dBrayCurtis}
\alias{dBrayCurtis}
\title{Bray Curtis Distance}
\description{ This function calculates Bray Curtis distance between pairs of individuals. }
\usage{
dBrayCurtis(x)
}
\arguments{
  \item{x}{ a data matrix with species abundance.}
  }
  
\details{ Bray Curtis distance is popular within  ecologists community. It quantifies differences between sampling locations when the ecological abundance of species is collected. It is recommended for species abundance data sets.


  \code{dBrayCurtis} allows to include missing values (such as NA). When a missing  value is present for an individual, the corresponding variable is excluded in calculating the distances for that individual. 
}
\value{It returns an object of class "\code{dist}".
}
\references{
Bray, J., Curtis, J. (1957). An ordination of upland forest communities of southern Wisconsin, \emph{Ecological monographs}, \bold{27}, 325--349.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}
\keyword{multivariate}

\seealso{\code{\link{dist}}, \code{\link{dBhatta}},  \code{\link{dcor}}, \code{\link{dGower}}, \code{\link{dHellinger}}, \code{\link{dMahal}}, \code{\link{dOrloci}}, \code{\link{dPrevosti}} }

\examples{
## Abundance Matrix of Multivariate sample without Missing Values:
sites <- 50
m <- 25
x <- matrix(0, sites, m)
for (s in 1:sites)
{
        x[s, ] <- tabulate(sample(1:m, round(runif(1, 100, 200)), replace=TRUE))
}
dBrayCurtis(x)

## Abundance Matrix of Multivariate sample and Missing Values:
x[1, 1] <- NA
dBrayCurtis(x)
}
