dPrevosti<-function(x){
        x <- as.matrix(x)
        if (any(x <0 , na.rm=TRUE))
        {
                stop("Your data must be >= 0")
        }
        if (any(is.na(x)))
        {
                missing <- TRUE
        }else{
                missing <- FALSE
        }
        if(any(x >1))
        {
                v <- colnames(x)
                locusnames <- sapply(v, strsplit, split="[.]")
                s <- sapply(locusnames, length)
                if (any(s!=2)){
                        stop("The name of each locus and corresponding alleles 
                     must be separeted by a single dot (.)")
                }
                if (missing)
                {
                     d <- dPrevostimissing(x)   
                }else
                {
                locusid <- sapply(locusnames, function(a){a[[1]]})
                aux <- table(locusid)
                nu <- length(aux)
                for (l in names(aux))
                {
                        selec <- locusid==l
                        xtemp <- x[, selec]
                        x[, selec] <- t(apply(xtemp, 1, function(f){f/sum(f)}))
                }
                
                d <- stats::dist(x, "manhattan")
                d <- d/(2*nu)
                }
        }else
        {

                d <- stats::dist(x, "manhattan")     
        }
        
        return(d)
}
