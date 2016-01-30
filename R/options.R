##' Set user preferences.  To make persistent, stored in inst/user
##' @title Set user preferences
##' @param path Path to default AFS directory
##' @param tracker Path to tracking file
##' @param cell AFS cell
##' @export
afs_set_opts <- function(path=NULL, tracker=NULL, cell=NULL) {
    if (is.null(path) && is.null(tracker)) return(invisible())
    prefs <- system.file('user/preferences.txt', package="sync.afs")
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
    if (!is.null(cell)) {
      lines[[4]] <- paste('cell:', cell)
    }
    cat(lines, file=prefs, sep="\n")
}

##' Read user options and set in options.
##' @title Read/set user options
##' @keywords internal
afs_read_opts <- function() {
    prefs <- system.file('user/preferences.txt', package="sync.afs")
    lines <- readLines(prefs)
    path <- trimws(sub('path:', '', lines[[2]], fixed=TRUE))
    tracker <- trimws(sub('tracker:', '', lines[[3]], fixed=TRUE))
    cell <- trimws(sub('cell:', '', lines[[4]], fixed=TRUE))
    options('afs.path'=path)
    options('afs.tracker'=tracker)
    options('afs.cell'=cell)
}
