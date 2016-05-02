summary.wdb <- function(object, show=TRUE, ...)
{
        conf <- object$conf
        etiq <- dimnames(conf)[[1]]
        K <- length(etiq)
        GC <- genercorre(conf, K)
        sen <- rep(NA, K)
        names(sen) <- etiq
        predval <-   rep(NA, K)
        names(predval) <- etiq
        spec <-   rep(NA, K)
        names(spec) <- etiq
        for (k in 1:K)
        {
                sen[k] <- conf[k,k]/sum(conf[k,])*100
                predval[k] <- conf[k,k]/sum(conf[,k])*100
                spec[k] <- sum(diag(conf[-k,-k, drop=FALSE]))/sum(conf[-k,])*100
        }
        
        if(show)
        {
        cat("Discriminant method: ", object$method, "\n")
        cat("------ Leave-one-out confusion matrix: ------\n")
        print(conf)
        p <- sum(diag(conf))/sum(conf)*100
        cat("\nTotal correct classification: ", format(p, digits=4), "% \n")  
        cat("\nGeneralized squared correlation: ", GC, "\n")   
        cat("\nSensitivity for each class: \n")  
        print(sen, digits=4)
        cat("\nPredictive value for each class: \n")  
        print(predval, digits=4)
        cat("\nSpecificity for each class: \n")  
        print(spec, digits=4)
        cat("------ ------ ------ ------ ------ ------\n")      
        if(is.null(object$pred))
        {
                cat(" \nNo predicted individuals")
        }else
        {
                cat("\nPrediction for new individuals: \n")
                print(object$pred)
        }
        }
        out <- rbind(sen, predval, spec)
        class(out) <- "summarywdb"
        invisible(out)
}
