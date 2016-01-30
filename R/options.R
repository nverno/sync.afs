##' Set user preferences.  To make persistent, stored in inst/user
##' @title Set user preferences
##' @param cell AFS cell
##' @param tracker Path to tracking file
##' @param userdir Path to users directory on AFS (after the cell)
##' @param key Data key (default to 'data_key.rda' in 'extdata')
##' @export
afs_set_opts <- function(cell, tracker, userdir, key) {
  prefs <- system.file('user/preferences.txt', package="sync.afs")
  lines <- readLines(prefs)
  
  ## Possible arguments
  if (!is.null(cell)) {
    lines[[2]] <- paste('cell:', cell)
    options('afs.cell'=cell)
  }
  if (!is.null(tracker)) {
    lines[[3]] <- paste('tracker:', tracker)
    options('afs.tracker'=tracker)
  }
  if (!is.null(userdir)) {
    lines[[4]] <- paste('userdir:', cell)
  }
  if (!is.null(key)) {
    lines[[5]] <- paste('key:', key)
  }
  cat(lines, file=prefs, sep="\n")
}

##' Read user options and set in options.
##' @title Read/set user options
##' @keywords internal
afs_read_opts <- function() {
  prefs <- system.file('user/preferences.txt', package="sync.afs")
  lines <- readLines(prefs)
  cell <- trimws(sub('cell:', '', lines[[2]], fixed=TRUE))
  tracker <- trimws(sub('tracker:', '', lines[[3]], fixed=TRUE))
  userdir <- trimws(sub('userdir:', '', lines[[4]], fixed=TRUE))
  key <- trimws(sub('key:', '', lines[[5]], fixed=TRUE))
  options('afs.cell'=cell)
  options('afs.tracker'=tracker)
  options('afs.userdir'=userdir)
  options('afs.key'=key)
  options('afs.path'=file.path('//afs', cell, userdir))
}
