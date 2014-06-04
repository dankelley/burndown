plot.burndown <- function(x, col=NULL, draw.plan=TRUE, draw.regression=FALSE, draw.lastupdate=FALSE, t.stop="",
                          mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0),
                          debug=FALSE, ...)
{
    if (!inherits(x, "burndown"))
        stop("method is only for burndown objects")
    num.items = length(x$tasks$key)
    num.progress = length(x$progress$key)
    if (is.null(col)) {
        col <- hcl(h = 360*(1:num.items)/num.items, c=70,l=80)
    }
    if (debug)
        cat("Progress:\n")
    t <- x$start
    effort.remaining <<- x$tasks$effort
    e <- effort.remaining
    if (debug) {
        cat("TIME:");print(t);cat("\n")
        cat("effort remaining:\n");print(effort.remaining);cat("\n")
        cat(sprintf("    %5s\t%20s\t%15s\n","Key","Percent Complete","Time"))
    }
    num.progress = length(x$progress$key)
    for (i in 1:num.progress) {
        if (debug) {
            cat(sprintf("    %5s\t%20s   ", x$progress$key[i], x$progress$progress[i]))
            cat(format(x$progress$time[i]))
            cat("\n")
        }
        t <- c(t, x$progress$time[i])
        k <- x$progress$key[i]
        effort.remaining[k] <- x$tasks$effort[k] * (1 - x$progress$progress[i]/100)
        if (debug) {
            cat(paste("k=",k,"\n"))
            cat("TIME:\n");print(x$progress$time[i]);cat("\n")
            cat("effort remaining:\n");print(effort.remaining);cat("\n")
        }
        e <- c(e,effort.remaining)
    }
    e.matrix <- matrix(e,ncol=num.items,byrow=TRUE) 
    if (t.stop != "") {
        time.max = as.POSIXct(t.stop)
    } else {
        time.max = x$deadline
    }
    par(mar=mar, mgp=mgp)
    plot(range(c(t[1],time.max)), range(c(0,sum(x$tasks$effort))),
         type='n',xlab="Time", ylab="Remaining Effort")
    xx <- c(t, rev(t))
    bottom <- rep(0,1+num.progress)
    for (i in 1:num.items) {
        y <- e.matrix[,i] + bottom
        yy <- c(y, rev(bottom))
        bottom <- y
        polygon(xx,yy,col=col[i])
    }
                                        # Indicate prediction (possibly with a regression line)
    total.effort <- c();
    for (i in 1:dim(e.matrix)[1])
        total.effort <- c(total.effort,sum(e.matrix[i,]))
    effort.anomaly <- total.effort - total.effort[1]
    t.anomaly <- t - t[1]
    m <- lm (effort.anomaly ~ t.anomaly - 1)
    slope <- m$coefficients[1][[1]]
    intercept <- total.effort[1] - slope * as.numeric(t[1])
    t.done <- floor(-intercept / slope)
    if (draw.regression)
        abline(a=intercept, b=slope, col="red",lwd=2,lty=2)
    class(t.done) <- "POSIXct"
    message("NOTE: predicted time of completion is ", format(t.done))
                                        # Indicate plan
    if (draw.plan) {
        lines(c(t[1],x$deadline),c(sum(x$tasks$effort),0),col="red",lwd=3)
        abline(h=0,col="red",lwd=3)
    }
    final.effort <-  sum(e.matrix[dim(e.matrix)[1],])
    if (draw.lastupdate) {
        points(t[length(t)],final.effort,col="yellow",cex=2.5,pch=19)
        points(t[length(t)],final.effort,col="blue",cex=2.6)
                                        #lines(c(t[length(t)],time.max),rep(final.effort,2),col=gray(0.9),lwd=3)#,col="red",lwd=3)
    }
    ll <- if (length(x$task$description) < 10) 0.8 else 0.6
    legend("topright",legend=rev(x$task$description),fill=rev(col),cex=ll,y.intersp=ll) 
}
