dBhattaNOmissing<-function (x) 
{
    x <- as.matrix(x)
    n <- dim(x)[1]
    p <- dim(x)[2]
    d <- matrix(0, nrow = n, ncol = n)
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            a <- sqrt(x[i, ] * x[j, ])
            a <- sum(a)
            d[i, j] <- acos(a)
            d[j, i] <- d[i, j]
        }
    }
    return(d)
}
