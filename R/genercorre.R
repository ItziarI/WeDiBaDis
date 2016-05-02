genercorre<-function(x,nclass){
        n <- sum(x)
        nr <- as.integer(nrow(x))
        nc <- as.integer(ncol(x))
        if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
            stop("invalid nrow(x) or ncol(x)", domain = NA)
        sr <- rowSums(x)
        sc <- colSums(x)
        E <- outer(sr, sc, "*")/n
   #     v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
   #     V <- outer(sr, sc, v, n)
   #     dimnames(E) <- dimnames(x)
        STATISTIC <- round(sum((abs(x - E)^2/E))/(n*(nclass-1)),4)
        if (is.nan(STATISTIC)) STATISTIC <- "no unit are classified in one or more classes"
        return( STATISTIC)
}    


       
