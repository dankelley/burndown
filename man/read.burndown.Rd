\name{read.burndown}
\alias{read.burndown}
\title{Scan burndown data file}
\description{
  Read a data file containing burndown information
}
\usage{
read.burndown(file, debug=FALSE)
}
\arguments{
  \item{file}{A connection or a character string giving the name of the
    file to load.}
  \item{debug}{Set to TRUE to print debugging information.}
}

\details{Reads a \code{burn} dataset, in a format described in the
    \code{\link{burn}} documentation.  Note that the progress list is ordered
    by time during the reading process.  }

\value{A burndown object.}

\seealso{
  \code{\link{summary.burndown}}
  \code{\link{plot.burndown}}
}
\examples{
library(burndown)
b <- read.burndown(system.file("extdata", "burn.dat", package="burndown"))
summary(b)
plot(b)
}

\references{
\url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.
}

\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
