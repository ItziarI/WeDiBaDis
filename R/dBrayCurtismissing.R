dBrayCurtismissing <- function(x)
{
        n <- dim(x)[1]
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
                        d[i, j] <- sum(abs(xi[m]-xj[m]))/(sum(xi[m])+sum(xj[m]))
                        d[j, i] <- d[i, j]
                }
        }
        return(d)
}