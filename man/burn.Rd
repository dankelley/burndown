\name{burn}

\docType{data}

\alias{burn}

\title{Sample burndown dataset}

\description{This is sample burndown dataset provided for testing.}

\usage{data(burn)}

\format{ A strict format is required, in which the following items must be
    present, in the stated order, and with nothing else in the file.  An
    example is given after the description.

Line 1: contains two items, first the string \code{Start}, and second a time in
ISO 8601 format (see below).

Line 2: contains two items, first the string \code{Deadline}, and second a time
in ISO 8601 format.

Line 3: blank

Line 4: a header line for a "task" list, comprising the following three words
separated by one or more spaces: \code{Key}, \code{Description}, and
\code{Effort}.

Lines 5 to N: a data line with three items, first a numeric index "Key" for the
task, second a "Description" of the task, and third the item's "Effort". The
keys must be distinct, and they must match the keys in the progress table (next
item). The description must be enclosed within the double-quote symbol. The
effort is a number that may represent e.g. the number of hours or days for the
task, or perhaps a percentage of the overall task.

Line N+1: blank

Line N+2: a header line for the "Progress" list, comprising the following four
words separated by one or more spaces: \code{Key}, \code{Done},  \code{Time},
and \code{Comment}.

Line N+3 to end: data lines holding Progress items. Each "Key" must match a key
in the task list, or an error will occur during \code{read.burndown()}. The
"Done" item is the percentage of completion of the task. The "Time" is in ISO
8601 format. Note that the list is ordered by time when it is read by
\code{\link{read.burndown}}.  The "Comment" is optional.

NB. an ISO 8601 date is formatted as "year-month-day", e.g. "2006-04-08" for
the 8th of April, 2006.  To add a time within the day, follow this by the
letter "T" and then the time in colon-separated format hour:minute:second. (The
second is optional.)

SAMPLE FILE

\preformatted{
Start		2006-04-08T12:00
Deadline	2006-04-11T20:00

Key Description                Effort
1   "code read.burndown()"          4
2   "code summary.burndown()"       1
3   "code plot.burndown()"          5
4   "create R package"              2
5   "write documentation"           2
6   "set up website"                1

Key    Done              Time               Comment
1        5          2006-04-08T13:00
2        5          2006-04-08T13:30
1       10          2006-04-08T14:00
2       50          2006-04-08T15:00
4        5          2006-04-08T19:30
5        5          2006-04-08T20:00
4      100          2006-04-08T21:16
1       50          2006-04-09T09:10
3        5          2006-04-09T09:41
3       10          2006-04-09T10:18
3       70          2006-04-09T11:00
2       60          2006-04-09T12:00
2       70          2006-04-09T12:10
1       70          2006-04-09T12:30
}
}

\references{ISO 8601 date format: \url{http://www.w3.org/TR/NOTE-datetime}}

\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}

\keyword{misc}
