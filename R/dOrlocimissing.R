dOrlocimissing <- function(x)
{
        n <- dim(x)[1]
        p <- dim(x)[2]
        d <- matrix(0, n, n)
        
        for (i in 1:(n-1))
        {
                for (j in (i+1):n)
                {
                        xi <- x[i, ]
                        mi <- sapply(xi, is.na)
                        xj <- x[j, ]
                        mj <- sapply(xj, is.na)
                        m <- !(mi | mj)
                        aux <- xi[m]/sqrt(sum(xi[m]^2)) - xj[m]/sqrt(sum(xj[m]^2))
                        d[i, j] <- sqrt(sum(aux^2)*p/sum(m))
                        d[j, i] <- d[i, j]
                }
        }
        return(d)
}