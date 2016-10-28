dPrevostimissing<-function(x){
       
        if(any(x >1))
        {
                v <- colnames(x)
                locusnames <- sapply(v, strsplit, split="[.]") 
                locusid <- sapply(locusnames, function(a){a[[1]]})
                aux <- table(locusid)
                nu <- length(aux)
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
                                for (l in names(aux))
                                {
                                        selec <- (locusid==l) & (m)
                                        aux1 <- xi[selec]/sum(xi[selec])
                                        aux2 <- xj[selec]/sum(xj[selec])
                                        d[i, j] <- d[i, j] + sum(abs(aux1 - aux2))
                                }   
                                d[j, i] <- d[i, j]
                        }
                }

                
                d <- d/(2*nu)
                d <- stats::as.dist(d)
                
                
        }else
        {

                d <- stats::dist(x, "manhattan")     
        }
        
        return(d)
}
