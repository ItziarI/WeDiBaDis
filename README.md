---
output:
  md_document:
    variant: markdown_github
---


WeDiBaDis
---------

`WeDiBaDis` package has two discriminant analysis procedures: the well-known distance-based discriminant analysis (DB-discriminant) and a novel classifier rule, the so-called weighted-distance-based discriminant (WDB-discriminant). This last procedure is based
on an improvement of the DB rule taking into account the statistical depth of the units.

Example of use
-----------

We are introducing the following fictitious situation to show a simple example of use of the main function `WDBdisc` in the packge. Imagine we have a data set where the units are classified in 3 classes. We simulated 20 units in dimension 5: 

```{r}
mu1 <- sample(1:10, 5, replace=TRUE)
x1 <- matrix(rnorm(20*5, mean = mu1, sd = 1),ncol=5, byrow=TRUE)
mu2 <- sample(1:10, 5, replace=TRUE)
x2 <- matrix(rnorm(20*5, mean = mu2, sd = 1),ncol=5, byrow=TRUE)
mu3 <- sample(1:10, 5, replace=TRUE)
x3 <- matrix(rnorm(20*5, mean = mu3, sd = 1),ncol=5, byrow=TRUE)
x <- rbind(x1,x2,x3)

#the right partition
classes <- c(rep(1,20), rep(2,20), rep(3,20))
```

We considered the Euclidean distance between units in matrix `x`.
```{r}
d <- as.matrix(dist(x))
```

In order to call the function `WDBdisc()` we need to build the object that includes the distance matrix and the class variable:

```{r}
join <- cbind(classes, d)
```

Now, we are ready to apply `WDBdisc()`:
```{r}
out <- WDBdisc(data=join, datatype="d", method="DB")
summary(out)
```

Imagine we have 3 units with unkown classes in object `x0` and we want to predict their classes. (We simulated one unit drawn from each class). We can use again the `WDBdisc()` function as follows:

```{r}
#Generation of x0
aux <- c(rnorm(5, mean = mu1, sd = 1), rnorm(5, mean = mu2, sd = 1), rnorm(5, mean = mu3, sd = 1))
x0 <- matrix(aux, ncol=5, byrow=TRUE)

# Prediction of classes
out <- WDBdisc(data=join, datatype="d", method="DB", new.ind=x0)
summary(out)
```

Installation
--------------
 Once package `devtools` is installed, it provides function `install_github()` to install the package.
```{r}
library(devtools)
install_github("ItziarI/WeDiBaDis")
```