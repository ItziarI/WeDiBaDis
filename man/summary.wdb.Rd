\name{summary.wdb}
\alias{summary.wdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary results for WDB classificacion 
}
\description{
Gives summary results  based on the confusion matrix (Leave-One-Out approach) obtained by  \code{\link{WDBdisc}}.
}
\usage{
 \method{summary}{wdb}(object, show=TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ object of class 'wdb' obtained by \code{\link{WDBdisc}}.}
  \item{show}{logical. If TRUE the results are printed (default value). }
  \item{\dots}{ arguments based from or to other methods.}
}


\details{
The following summarizing measures are calculated:
the generalized squared correlation  and the Cohen's kappa coefficient which are performance measures in a classification task, adequate for K>=2 classes; sensibility, specificity, predicted value and F1-score for each class.

It can be used with a \code{plot} method and it creates a barplot with sensibility, specificity and predicted value for each class.
}
\references{
Baldi P., Brunak S., Chauvin Y., Andersen C., Nielsen H. (2000) Assessing the accuracy of prediction algorithms for classification: an overview. \emph{ Bioinformatics}, \bold{ 16}, 412--424.

Matthews B. (1975) Comparison of the predicted and observed secondary structure of T4 Phage Lysozyme. \emph{Biochimica Biophysica Acta}, \bold{405}, 442--451.

Cohen J. (1960) A Coefficient of Agreement for Nominal Scales. \emph{Educational and Psychological Measurement}, \bold{20}, 37--46.
}

\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}

\value{ A matrix with   sensitibitily, specificity and predictive value of each class is returned.
} 
 
 

\seealso{
  \code{\link{WDBdisc}}
}
\examples{
\dontrun{
data("corearea")

out <- WDBdisc(data=corearea, datatype="m", distance="euclidean", method="WDB")
summary(out)
# Barplot
plot(summary(out, show=FALSE))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivarite }

