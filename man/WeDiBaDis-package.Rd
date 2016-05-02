\name{WeDiBaDispackage}
\alias{WeDiBaDispackage}
\docType{package}
\title{Weighted Distance Based Discriminant Analysis (WeDiBaDis)}
\description{\code{WeDiBaDis} is a package that helps to performe a Distance-Based (DB) and a weighted-distance-based (WDB) discriminant analysis.
}
\details{
\tabular{ll}{
Package: \tab WeDiBaDis\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2016-01-13\cr
License: GPL (>=2)\cr
}
WeDiBaDis package includes the principal function \code{WDBdisc} to perform DB and WDB analysis. 

It can be used over a data matrix or over a distance matrix. Missing values (such as NA) are allowed and the following different metrics are available:

\code{"euclidean"} (\code{dist}), \code{"correlation"} (\code{dcor}), 
\code{"Bhattacharyya"} (\code{dBhatta}), "Gower" (\code{dGower}) , "Mahalanobis" (\code{dMahal}), "BrayCurtis" (\code{dBrayCurtis}), "Orloci" (\code{dOrloci}), "Hellinger" (\code{dHellinger}), 
and "Prevosti" (\code{dPrevosti}).

\code{WDBdisc} returns an object of class "wdb" and the corresponding \code{plot} and \code{summary} methods provides the following results:
 The classification table obtained with the Leave-One-Out cross-validation; The total well classifcation rate in percentage; The generalized squared correlation; The sensitivity, specificity and positive predictive values for each class; The assigned class for new unlabeled units to be classified; A barplot for the classifaction table; A barplot for the sensitivity, specificity and positive predictive values for each class.
}

\author{
I. Irigoien, F. Mestres and C. Arenas
Maintainer: I. Irigoien <itziar.irigoien@ehu.eus>
}
\references{
Irigoien, I., Mestres, F., Arenas, C. \emph{Weighted Distance Based Discriminant Analysis: The R package WeDiBaDis}, \bold{vol}, 00--00.
}
\keyword{ package }
