dMahalmissing<-function (x, S) 
{
    n <- dim(x)[1]
    p <- dim(x)[2]
    d <- matrix(0, n, n)
    for (i in 1:n) {
        for (j in 1:i) {
            xi <- x[i, ]
            mi <- sapply(xi, is.na)
            xj <- x[j, ]
            mj <- sapply(xj, is.na)
            m <- !(mi | mj)
            dif <- xi[m] -xj[m]
            dif <- matrix(dif, ncol = 1)
            Sm <- S[m,m]
            Sminv <- solve(Sm)
            d[i, j] <- sqrt((t(dif) %*% Sminv %*% dif)*p/sum(m))
            d[j, i] <- d[i, j]
        }
    }
    d <- as.dist(d)
    return(d)
}
