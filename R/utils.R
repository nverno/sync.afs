## Return the afs path for linux or on windows
##' @title get_afs
##' @export
get_afs <- function() {
  if (Sys.info()[['sysname']]=='Linux')
    afs.linux
  else
    afs
}
