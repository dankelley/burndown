trim <- function(s)
{
    ns <- length(s)
    ss <- vector("character", ns)
    for (i in 1:ns) 
        ss[i] <- gsub("\\s*$", "", gsub("^\\s*", "", s[i]))
    ss
}

read.burndown <- function(file, debug=FALSE)
{
    fix.time <- function(time.with.T) 
    {
        return(as.POSIXct(gsub("T", " ", time.with.T)))
    }
    quiet <- if (debug) FALSE else TRUE;

    split.on <- "[ \t]+"
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    tokens <- trim(scan(file, what='char', sep=",", nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    if (tokens[1] != "Start")
        stop("First line of file must be 'Start' followed by a comma and then an ISO date.\n")
    start <- fix.time(tokens[2])
    tokens <- trim(scan(file, what='char', sep=",", nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    if (tokens[1] != "Deadline")
        stop("Second line of file must be 'Deadline' followed by a comma and then an ISO date.\n")
    deadline <- fix.time(tokens[2])
    blank.line <- scan(file, character(), nlines=1, quiet=quiet)
    ## Get and check header
    a.line <- trim(scan(file, character(), sep=",", nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    if (a.line[1] != "Key") {
        stop("Looking for 'Key,Description,Effort' but got '",a.line,"' (missing Key)")
    }
    if (a.line[2] != "Description")
        stop(paste("Looking for 'Key,Description,Effort' but got '",a.line,"' (missing Description)\n",sep=""))
    if (a.line[3] != "Effort")
        stop(paste("Looking for 'Key,Description,Effort' but got '",a.line,"' (missing Effort)\n",sep=""))
    task.key <- c()
    task.description <- c()
    task.effort <- c()
    while (TRUE) { # TASK: key description effort
        a.line <- trim(scan(file, what=character(), sep=",",
                            nlines=1, blank.lines.skip=FALSE, quiet=quiet))
        if (3 == length(a.line)) {
            task.key         <- c(task.key,         as.numeric(a.line[1]))
            task.description <- c(task.description, a.line[2])
            task.effort      <- c(task.effort,      as.numeric(a.line[3]))
        } else {
            if (a.line == "") {
                break;
            } else {
                stop(paste("Need 3 items, but got the following line:\n\t'",a.line,"'\n",sep=""))
            }
        } 
        if (debug) {
            cat(paste("key:",a.line[1]," description",a.line[2]," effort", a.line[3]," comment", a.line[4],"\n"))
        }
    }
    if (debug) {
        cat("task.key:");         print(task.key)
        cat("task.description:"); print(task.description)
        cat("task.effort:");      print(task.effort)
    }
    ## "Key	Progress Time Comment" + data lines
    names <- trim(scan(file, character(), sep=",", nlines=1, quiet=quiet))
    if (names[1] != "Key")
        stop("First item should be Key but it is", names[1])
    if (names[2] != "Done")                 
        stop("Second item should be Key but it is", names[2])
    if (names[3] != "Time")                     
        stop("Third item should be Key but it is", names[3])
    if (names[4] != "Comment") {
        stop("Fourth item should be Key but it is", names[4])
    }
    progress.key     <- c()
    progress.done    <- c()
    progress.time    <- c()
    progress.comment <- c()
    while (TRUE) {
        a.line <- scan(file, what=character(), sep=",", 
                       nlines=1,blank.lines.skip=FALSE,quiet=quiet)
        l <- length(a.line)
        if (3 == l) {
            comment <- ""
        } else {
            if (4 == l) {
                comment <- a.line[4]
            } else {
                break; # BUG: should check to see if it's partial data
            }
        }
        key <- as.numeric(a.line[1])
        if (!(key %in% task.key)) {
            msg <- paste("Progress key",key,"not in the list of task keys\n\tOffending line in data file follows\n\t",a.line[1]," ",a.line[2], " ",a.line[3])
            stop(msg)
        }
        done <- as.numeric(a.line[2])
        time <- fix.time(a.line[3])
        progress.key     <- c(progress.key,     key)
        progress.done    <- c(progress.done,    done)
        progress.time    <- c(progress.time,    time)
        progress.comment <- c(progress.comment, comment)
        if (debug) {
            cat(paste("key=",key," done=",done," time=",time," comment=",comment,"\n",sep=""))
        }
    }
    class(progress.time) <- "POSIXct"
                                        # BUG: should ensure item is in task
    if (debug) {
        cat("Progress.key:    ");		print(progress.key)
        cat("Progress.done:   ");		print(progress.done)
        cat("Progress.time:   ");		print(progress.time)
        cat("Progress.comment:");		print(progress.comment)
    }      
    o <- order(progress.time)
    progress.key     <- progress.key[o]
    progress.done    <- progress.done[o]
    progress.time    <- progress.time[o]
    progress.comment <- progress.comment[o]
    rval <- list(start=start,
                 deadline=deadline,
                 tasks = list(key=task.key,
                              description=task.description,
                              effort=task.effort),
                 progress = list(key=progress.key,
                                 progress=progress.done,
                                 time=progress.time,
                                 comment=progress.comment))
    class(rval) <- "burndown"
    return(rval)
}
