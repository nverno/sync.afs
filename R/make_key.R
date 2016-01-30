##' Create data_key (as data.table) to store files/locations/info for quicker access
##'
##' @param path Path to root directory.
##' @param tracker Path to data tracking file (default getOption('afs.tracker'))
##' @importFrom tools file_ext file_path_sans_ext
##' @examples
##' \dontrun{
##'   data_key <- create_data_key_template()
##' }
##' @return data.table
##' @export
create_data_key_template <- function(path=getOption('afs.path'),
                                     tracker=getOption('afs.tracker')) {
  dat <- process_tracker(tracker=tracker)

  data_key <- file_info(path=path,
    files=c(dat$files, 
      unlist(lapply(dat$renamed, `[[`, 2), use.names=FALSE)), 
    rnames=NA_character_)
  return( data_key[] )
}
