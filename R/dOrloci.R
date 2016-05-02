dOrloci<-function(x){
        if (any(x < 0, na.rm = TRUE))
        {
                stop("Your data must be abundances")
        }
        if (any(is.na(x)))
        {
                missing <- TRUE
        }else{
                missing <- FALSE
        }
        
        if (missing)
        {
                d <- dOrlocimissing(x)
        }else{
                y <- t(apply(x, 1, function(f){f/sqrt(sum(f^2))}))
                d <- dist(y)
        }     
        
        d <- as.dist(d)
        return(d)

}





