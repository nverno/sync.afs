##' @export
get_afs <- function() {
  if (Sys.info()[['sysname']]=='Linux')
    afs.linux
  else
    afs
}
