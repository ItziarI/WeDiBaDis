summary.wdb <- function(object, show=TRUE, ...)
{
        conf <- object$conf
        etiq <- dimnames(conf)[[1]]
        K <- length(etiq)
        GC <- genercorre(conf, K)
        kappa <- kappacohen(conf)
        sen <- rep(NA, K)
        names(sen) <- etiq
        predval <-   sen
        spec <-   sen
        F1 <- sen
        for (k in 1:K)
        {
                sen[k] <- conf[k,k]/sum(conf[k,])*100
                predval[k] <- conf[k,k]/sum(conf[,k])*100
                spec[k] <- sum(diag(conf[-k,-k, drop=FALSE]))/sum(conf[-k,])*100
                F1[k] <- 2*predval[k]*sen[k]/(predval[k] + sen[k])
        }
        
        if(show)
        {
        cat("Discriminant method: ", object$method, "\n")
        cat("------ Leave-one-out confusion matrix: ------\n")
        print(conf)
        p <- sum(diag(conf))/sum(conf)*100
        cat("\nTotal correct classification: ", format(p, digits=4), "% \n")  
        cat("\nGeneralized squared correlation: ", GC, "\n")   
        cat("\nCohen's Kappa coefficient: ", kappa, "\n")   
        cat("\nSensitivity for each class: \n")  
        print(sen, digits=4)
        cat("\nPredictive value for each class: \n")  
        print(predval, digits=4)
        cat("\nSpecificity for each class: \n")  
        print(spec, digits=4)
        cat("\nF1-score for each class: \n")  
        print(F1, digits=4)
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
