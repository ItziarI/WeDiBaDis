\name{dPrevosti}
\alias{dPrevosti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prevosti distance
}
\description{
It calculates the Prevosti distance for genetic data.
}
\usage{
dPrevosti(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data
}
}
\details{
To identify properly which are the locus and its corresponding alleles each column of \code{x} must be named. The name must have two parts: a first one to identify the locus and a second one to  identify the allele. Both parts must be separated with a dot (.). For instance, names "INRA5.133", "INRA5.137", "INRA5.139", and  "INRA5.141" are indicating 4 different alleles (133, 137, 139 and 141) for locus "INRA5".
}
\value{
It returns an object of class \code{\link{dist}}.
}
\references{
Prevosti A., Oca\~na J. and Alonso G. (1975) Distances between populations fo Dropsophila Subobscura, based on chromosome arrangement frequencies. \emph{Theoretical and Applied Genetics}, \bold{45}, 231--241.
}
\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}


\seealso{
\code{\link{dist}}, \code{\link{dBhatta}}, \code{\link{dBrayCurtis}}, \code{\link{dcor}}, \code{\link{dGower}}, \code{\link{dHellinger}}, \code{\link{dMahal}}, \code{\link{dOrloci}} 
}
\examples{
\dontrun{
data(microsatt)
dPrevosti(microsatt)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
