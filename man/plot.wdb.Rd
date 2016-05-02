\name{plot.wdb}
\alias{plot.wdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot results of WDB classificacion 
}
\description{
Creates a barplot with   the predicted classes by  \code{\link{WDBdisc}}. Each bar corresponds to a class and for units belonging to this class the distribution of the predicted classes is shown within the bar.
}
\usage{
 \method{plot}{wdb}(x, leg=TRUE, col=NULL, \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class 'wdb' obtained by \code{\link{WDBdisc}}.}
  \item{leg}{logical. If TRUE a legend will be placed at the top (default value). }
  \item{col}{a vector of colors for the bars or bar components. By default, \code{rainbow} is used.}
  \item{\dots}{ arguments based from or to other methods.}
}



\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}


\seealso{
 \code{\link{barplot}}, \code{\link{WDBdisc}}
}
\examples{
\dontrun{
data("corearea")

out <- WDBdisc(data=corearea, datatype="m", distance="euclidean", method="WDB")
plot(out)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivarite }

