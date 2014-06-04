\name{plot.burndown}
\alias{plot.burndown}
\title{Scan burndown data file}
\description{
  Plot a burndown object
}
\usage{\S3method{plot}{burndown}(x, col=NULL, draw.plan=TRUE, draw.regression=FALSE,
              draw.lastupdate=FALSE, t.stop="", debug=FALSE, ...)
}
\arguments{
  \item{x}{A burndown object.}
  \item{col}{Colours for items, starting with the first key in the 
	file (which will be at the bottom of the chart).  If not specified,
	the \code{\link{hcl}} scheme will be used, to generate colours
	that are distinct, that show up reasonably well on a monitor.} 
  \item{draw.plan}{Set TRUE to draw the plan, as a descending with a horizontal intercept.}   
  \item{draw.regression}{Set TRUE to draw a regression line of the realized effort.}   
  \item{draw.lastupdate}{Set TRUE to draw the last update (which otherwise requires a sharp eye).}   
  \item{t.stop}{Maximum time for graph (defaults to deadline if not given)}
  \item{debug}{Set TRUE to monitor the work.}
  \item{...}{Extra things handed down.}
}
\details{
  Plots a burndown chart.
}
\value{
  No return value.
}

\seealso{
\code{\link{read.burndown}}
\code{\link{summary.burndown}}
}
\examples{
library(burndown)
data(burn)
summary(burn)
plot(burn)
}
\references{
\url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
