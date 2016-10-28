dBhatta<-function (x) 
{

        
        if (any(is.na(x)))
        {
                missing <- TRUE
        }else{
                missing <- FALSE
        }
        
        if (missing)
        {
            d <- dBhattamissing(x)
        }else{
                one <- apply(x, 1, sum, na.rm=TRUE)
                if(any(sapply(one, function(a){ifelse((a > 0.99)&(a < 1.01), FALSE, TRUE)})))
                {
                        stop("The sum of each data row must be equal to 1.\nAt least for one row the sum is not in (0.9, 1.1)")
                }else
                {
                        x <- x/one
                }
            d <- dBhattaNOmissing(x)
        }
   d <- stats::as.dist(d)
   return(d)    

}
