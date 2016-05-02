dBrayCurtis<-function(x){
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
                d <- dBrayCurtismissing(x)
        }else{
                d <- dBrayCurtisNOmissing(x)
        }     

    d <- as.dist(d)
    return(d)
}
