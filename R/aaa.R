##' Read user options and set in options.
##' @title Read/set user options
##' @keywords internal
afs_read_opts <- function() {
    prefs <- system.file('inst/user/preferences.txt', package="sync.afs")
    ## prefs <- 'inst/user/preferences.txt'
    lines <- readLines(prefs)
    path <- substr(lines[[2]], 7, nchar(lines[[2]]))
    tracker <- substr(lines[[3]], 10, nchar(lines[[3]]))
    options('afs.path'=path)
    options('afs.tracker'=tracker)
}

.onAttach <- function(...) {
    afs_read_opts()
}
