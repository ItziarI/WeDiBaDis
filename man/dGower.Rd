\name{dGower}
\alias{dGower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gower Distance 
}
\description{
It calculates de Gower distance for mixed variables.
}
\usage{
dGower(x, type=list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data matrix.}
  \item{type}{it is a list with components \code{cuant}, \code{bin}, \code{nom}. Each component must indicate the column position of the quantitative, binary or nominal variables, respectively.}
}
\details{
The Gower distance between two units is given by: \eqn{d_{ij}= \sqrt{2*(1-s_{ij})}}, where \eqn{s_{ij}} is the Gower similarity coefficient for mixed data. This function allows to include missing values (as \code{NA}) and therefore calculates distances based on the  Gower's weighted similarity coefficient.
}
\value{
 It is an object of class \code{\link{dist}}.
}
\references{
 Gower, J.C. (1971). A general coefficient of similarity and some of its properties. \emph{Biometrics}, \bold{27}, 857--871.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}

\keyword{multivariate}

\seealso{\code{\link{dist}}, \code{\link{dBhatta}}, \code{\link{dBrayCurtis}}, \code{\link{dcor}},  \code{\link{dHellinger}}, \code{\link{dMahal}}, \code{\link{dOrloci}}, \code{\link{dPrevosti}} }

\examples{
# Generate some cuantitative, binary and nominal variables at random
x1 <- matrix(rnorm(20), 5)
x2 <- matrix(sample(0:1, 15, replace=TRUE), 5)
x3 <- matrix(sample(1:3, 15, replace=TRUE), 5)
x <- cbind(x1, x2, x3)
dGower(x, type=list(cuant=1:4, bin=5:7, nom=8:10))

# If missing values are present
x[2, 1] <- NA
x[3, c(6, 9)] <- NA
dGower(x, type=list(cuant=1:4, bin=5:7, nom=8:10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

