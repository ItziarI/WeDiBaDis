dcor<-function(x) 
{
        if (any(is.na(x)))
        {
                missing <- TRUE
        }else
        {
                missing <- FALSE
        }
        
        if (missing)
        {
                d <- dcormissing(x)
        }else
        {
                aux<-t(x)
                d<-sqrt(1-cor(aux))
        }
        d <- as.dist(d)
        return(d)    

}
