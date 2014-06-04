\name{burndown}
\docType{package}
\alias{burndown-package}
\title{The burndown package}
\description{This package creates burndown charts, used in project planning.}

\details{ Burndown charts are used in project planning to monitor task
    completion with respect to a deadline.

The format is simple. The x axis represents time, ranging from the onset of the
project to the deadline for completion. The y axis represents the remaining
effort that is require to accomplish the work. As the work is carried out, the
chart provides a running summary of progress towards the deadline.

If work is proceeding smoothly according to schedule, the chart takes the form
of a triangle, with the remaining effort falling from its initial value to zero
at the deadline. For guidance, a gray line is drawn to indicate this ideal
situation. If work is being accomplished faster than expected, the data will
lie below this gray line. However, if the project is falling behind schedule,
the data line will lie above the ideal line.  Thus, a glance at the chart
indicates whether the deadline can be met.

In most cases, the work is divided into sub-tasks, and the remaining effort in
each task is shown with a different colour in the chart. This is helpful in
identifying tasks that may need more attention.

The example dataset in this package is the burndown chart for the package
itself. See \code{\link{burn}} to learn more about the format.  Burndown
datasets are normally read with \code{\link{read.burndown}}, summarized with
\code{\link{summary.burndown}} (which may be abbreviated \code{summary}), and
plotted with \code{\link{plot.burndown}} (which may be abbreviated
\code{plot()}). 

PS. if you want Gantt charts, use the R package \code{gantt.chart} in the
package \code{plotrix}.

}

\references{ Burndown charts are discussed at the following site:
    \url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.
}

\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}

\keyword{misc}
