summary.burndown <- function(object, ...)
{
	if (!inherits(object, "burndown"))
		stop("method is only for burndown objects")
	cat(paste("START TIME\n\t",object$start,"\n"))
	cat(paste("DEADLINE\n\t",object$deadline,"\n"))
	cat("\nTASKS\n");
	cat(sprintf("    %5s\t%30s\t%5s\n","Key","Description","Effort"))
	num.tasks = length(object$tasks$key)
	for (i in 1:num.tasks) {
		cat(sprintf("    %5s\t%30s\t%5s\n",
			object$tasks$key[i], object$tasks$description[i], object$tasks$effort[i]))
	}
	cat("\nPROGRESS\n");
	cat(sprintf("    %5s\t%20s\t%10s\n","Key","Percent Complete","Time"))
	num.progress = length(object$progress$key)
	for (i in 1:num.progress) {
		cat(sprintf("    %5s\t%20s   ", object$progress$key[i], object$progress$progress[i]))
		cat(format((object$progress$time[i])))
		cat("\n")
	}
}
