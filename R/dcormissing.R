dcormissing <- function(x)
{
 n <- dim(x)[1]
 p <- dim(x)[2]
 d <- matrix(0, n, n)
 for (i in 1:(n-1))
 {
    for(j in (i+1):n)
    {
            xi <- x[i, ]
            mi <- sapply(xi, is.na)
            xj <- x[j, ]
            mj <- sapply(xj, is.na)
            m <- !(mi | mj)
            d[i, j] <- sqrt(1-cor(xi[m], xj[m]))
            d[j, i] <- d[i, j]
    }
 }
 return(d)
}
