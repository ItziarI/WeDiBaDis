plot.wdb <- function(x, leg=TRUE, col=NULL, inset=-0.1, ...)
{
        if (!inherits(x, "wdb")) 
                stop("use only with \"wdb\" objects")
        conf <- x$conf
        etiq <- dimnames(conf)[[1]]
        conf.p <- t(apply(conf, 1, function(aux){aux/sum(aux)}))*100
        if (is.null(col)) 
                col <- grDevices::rainbow(length(etiq))
        op <- graphics::par("mar"=c(4,4,5,2))
        on.exit(graphics::par(op))
        graphics::barplot(t(conf.p), names.arg=etiq, beside=FALSE, col=col, ...)
        graphics::mtext("Classification Table", 3, line=2)
        graphics::mtext( "Classes", 1, line=2)
        graphics::mtext("Percentage of classification in each class", 2, line=2)
        if(leg)
        {
            graphics::legend("top",legend=etiq, col="black", pch=21, pt.bg=col, text.col=col, 
                   xpd=TRUE, horiz=TRUE, inset=inset, bty="n", ...)
        }
}