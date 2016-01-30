##' Set user preferences.  To make persistent, stored in inst/user
##' @title Set user preferences
##' @param path Path to default AFS directory
##' @param tracker Path to tracking file
##' @export
afs_set_opts <- function(path=NULL, tracker=NULL) {
    if (is.null(path) && is.null(tracker)) return(invisible())
    prefs <- system.file('inst/user/preferences.txt', package="sync.afs")
    lines <- readLines(prefs)
    
    ## Possible arguments
    if (!is.null(path)) {
        lines[[2]] <- paste('path:', path)
        options('afs.path'=path)
    }
    if (!is.null(tracker)) {
        lines[[3]] <- paste('tracker:', tracker)
        options('afs.tracker'=tracker)
    }
    cat(lines, file=prefs, sep="\n")
}
