kappacohen <-function(x){
        x <- as.matrix(x)
        n <- sum(x)
        nr <- as.integer(nrow(x))
        nc <- as.integer(ncol(x))
        if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
                stop("invalid nrow(x) or ncol(x)", domain = NA)
        sr <- rowSums(x)
        sc <- colSums(x)
        Pe <- sum(sr*sc)/(n*n)
        Pa <- sum(diag(x))/n       
        STATISTIC <- (Pa-Pe)/(1-Pe)
        return( STATISTIC)
}    
