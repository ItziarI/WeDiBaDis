dMahal<-function (x, tol=1e-8) 
{
        if (any(is.na(x)))
        {
                missing <- TRUE
        }else{
                missing <- FALSE
        }
        S <- stats::cov(x, use="complete.obs")
        vd <- svd(S)$d
        if(min(abs(vd)) < tol)
        {
                stop( "The covariance matrix is singular" )     
        }
        Sinv <- solve(S)
        
        if (missing)
        {
                d <- dMahalmissing(x, Sinv)
        }else{
               
                d <- dMahalNOmissing(x, S)
        }     
    d <- stats::as.dist(d)
    return(d)
}
