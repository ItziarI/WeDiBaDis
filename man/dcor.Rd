\name{dcor}
\alias{dcor}
\title{ Correlation distance}
\description{ This function calculates the correlation distance based on Pearson coefficient.}
\usage{
dcor(x)
}
\arguments{
  \item{x}{ a data matrix.}
  }
  \details{ \code{dcor} calculates the correlation distance \eqn{d=\sqrt{1-r}} where \eqn{r} is the Pearson correlation. Missing values will be indicated by NA. When a missing  value is present for an individual, the corresponding variable is excluded in calculating the distances for that individual. It uses function \code{\link{cor}} from package \code{stats}.
}
\value{It returns an object of class "\code{dist}".
}
\references{
 Gower, J.C. (1985). Measures of similarity, dissimilarity and distance. 
In: \emph{Encyclopedia of Statistical Sciences}, volume \bold{5}, 397--405. J. Wiley and Sons.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}

\keyword{multivariate}
\seealso{\code{\link{dist}}, \code{\link{dBhatta}}, \code{\link{dBrayCurtis}},  \code{\link{dGower}}, \code{\link{dHellinger}}, \code{\link{dMahal}}, \code{\link{dOrloci}}, \code{\link{dPrevosti}} 
}
\examples{## Matrix of Multivariate sample without Missing Values:
n.var <- 100
n.samples <- 50
x <- matrix(rnorm(n.samples*n.var), ncol = n.var)
#correlation distance
dcor(x)

## Matrix of Multivariate sample with Missing Values:
n.var <- 100
n.samples <- 50
x <- matrix(rnorm(n.samples*n.var), ncol = n.var)
x[1,1] <- NA
#correlation distance
dcor(x)
}

