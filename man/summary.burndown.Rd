\name{summary.burndown}
\alias{summary.burndown}
\title{Summarize a burndown object}
\description{
  Summarizes a burndown object.
}
\usage{\S3method{summary}{burndown}(object, ...)
}
\arguments{
\item{object}{A \code{burndown} object, e.g. as read by \code{\link{read.burndown}}.}
\item{...}{Extra arguments (not used in this version).}
}
\details{
  Prints a summary of a burndown dataset.
}
\value{
  No return value.
}

\seealso{
  The \code{burndown} object may be read with \code{\link{read.burndown}}.
}
\examples{
library(burndown)
data(burn)
summary(burn)
}
\references{
\url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
