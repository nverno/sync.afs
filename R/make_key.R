##' Create data_key (as data.table) to store files/locations/info for quicker access
##' @title Initialize/recreate data_key from scratch (resets R data names to default).
##' @param path Path to root directory from which to search.
##' @param tracker Path to data tracking file.
##' @importFrom tools file_ext file_path_sans_ext
##' @import data.table
##' @examples
##' \dontrun{
##'   data_key <- create_data_key_template()
##' }
##' @return data.table
##' @export
create_data_key_template <- function(path=get_afs(),
                                     tracker=file.path(get_afs(), "file_tracker.txt")) {
  dat <- process_tracker(tracker)

  ## Get file info
  data_key <- file_info(path=path,
                        files=c(dat$files, unlist(lapply(dat$renamed, `[[`, 2),
                          use.names=FALSE)), rnames=NA_character_)
  
  return( data_key[] )
}
