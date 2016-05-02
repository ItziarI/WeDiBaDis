\name{WDBdisc}
\alias{WDBdisc}
\alias{print.wdb}

\title{ Distance-Based (DB) and Weighted-Distance-Based (WDB) discriminant analysis}
\description{ Given a data set or a distance matrix, it performs Distance-Based (DB) and Weighted-Distance-Based (WDB) discriminant analysis. 
}
\usage{
WDBdisc(data, datatype, new.ind=NULL, distance="euclidean", method, type)
}
\arguments{
  \item{data}{ a data matrix or a distance matrix, with the restriction that the first column must contain the class labels.}
  \item{datatype}{ if the data is a data matrix, datatype="m"; if the data is a distance matrix datatype="d".}
  \item{new.ind}{ NULL if there are not new individuals to classify (default value). When there are new individuals to classify: if datatype="m", it is a matrix containing the feature values for the new units to be classified; if datatype="d" it is a matrix containing each row the distances between the new unit to be classified and the units in the classes.}
  \item{distance}{ the distance measure to be used. This must be one of "euclidean"(default value), "correlation", "Bhattacharyya", "Gower", "Mahalanobis", "BrayCurtis", "Orloci", "Hellinger", or "Prevosti".}
  \item{type}{ only required if distance = "Gower". \code{type} is a list object \code{type = list(cuant,nom,bin)} indicating the column positions in the data matrix of continuous (cont), nominal (nom) and binary (bin) features, respectively.}
  \item{method}{ the discriminant method to be used. This must be one of "DB", "WDB" for the DB-discriminant and WDB-discriminant, respectively}
  }
\details{It performs the Distance-Based (DB)  discriminant analysis based on the proximity function or the Weighted-Distance-Based (WDB) discriminant analysis combining the proximity function and the depth function of a unit with respect to a class. 
To assess the performance of the classification obtained the Leave-One-Out (LOO) approach is used. It has associated a \code{summary} method to obtain the generalized squared correlation as well as sensitivity, speficity and positive predictive value for each class.
The associated \code{plot} method gives a barplot of the LOO confunsion matrix.
}
\value{This function returns an object of class \code{wdb} which is a \code{list} with:
  \item{conf}{ The classification table obtained with the leave-one-out cross-validation.}
  \item{pred}{ If \code{new.ind} is not \code{NULL}, the assigned class for new unlabeled units to be classified.}
  \item{method}{ The discriminant method used, DB or WDB}
}
\references{
Cuadras, C.M., Fortiana, J., Oliva, F. (1997) The proximity of an individual to a population with applications to discriminant analysis. \emph{Journal of Classification}, \bold{14}, 117--136. 

Irigoien I., Mestres F., Arenas C. (2013)The depth problem: identifying the most representative units in a data group. \emph{IEEE/ACM Transactions on Computational Biology and Bioinformatics}, \bold{10}, 161--172.
}

\author{
I. Irigoien \email{itziar.irigoien@ehu.eus}; Department of Computation Science and Artificial Intelligence, University of the Basque
Country, Donostia, Spain

F. Mestres \email{fmestres@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Genetics. University of Barcelona, Barcelona, Spain

C. Arenas \email{carenas@ub.edu}; Department of Genetics, Microbiology and Statistics. Section of Statistics. University of Barcelona, Barcelona, Spain
}

\keyword{multivariate}

\seealso{\code{\link{plot.wdb}}, \code{\link{summary.wdb}} }


\examples{
\dontrun{
# A data set without new unlabeled units to predict
data(corearea)

out <- WDBdisc(data=corearea, datatype="m", 
               distance="euclidean", method="WDB")
summary(out)


# A data set with new unlabeled units 
selec <- seq(5, 40, by=5)
train <- corearea[-selec, ]
test <- corearea[selec, -1]
out <- WDBdisc(data=train, datatype="m", new.ind=test, 
               distance="euclidean", method="WDB")
summary(out)}

# A distance matrix as input 
# Generate 3 classes, each of them with 20 units in dimension 5.
mu1 <- sample(1:10, 5, replace=TRUE)
x1 <- matrix(rnorm(20*5, mean = mu1, sd = 1),ncol=5, byrow=TRUE)
mu2 <- sample(1:10, 5, replace=TRUE)
x2 <- matrix(rnorm(20*5, mean = mu2, sd = 1),ncol=5, byrow=TRUE)
mu3 <- sample(1:10, 5, replace=TRUE)
x3 <- matrix(rnorm(20*5, mean = mu3, sd = 1),ncol=5, byrow=TRUE)
x <- rbind(x1,x2,x3)

# Euclidean distance between units in matrix x.
d <- as.matrix(dist(x))
# given the right partition
classes <- c(rep(1,20), rep(2,20), rep(3,20))
join <- cbind(classes, d)

out <- WDBdisc(data=join, datatype="d", method="DB")
summary(out)

}
