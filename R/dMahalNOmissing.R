dMahalNOmissing<-function (x, Sinv) 
{
    n <- dim(x)[1]
    d <- matrix(0, n, n)
    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            a <- x[i, ] - x[j, ]
            a <- matrix(a, ncol = 1)
            d[i, j] <- sqrt(t(a) %*% Sinv %*% a)
            d[j, i] <- d[i, j]
        }
    }
  
    return(d)
}
