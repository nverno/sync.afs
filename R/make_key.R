## Create data_key (as data.table) to store files/locations/info for quicker access
##' @title Create data_key
##' @param path path to file tracking document
##' @importFrom tools file_ext file_path_sans_ext
##' @import data.table
##' @examples
##' \dontrun{
##'   data_key <- create_data_key_template()
##' }
##' @return data.table
##' @export
create_data_key_template <- function(tracker=file.path(get_afs(), "file_tracker.txt")) {
  dat <- process_tracker(tracker)

  ## Get file info
  data_key <- file_info(files = c(dat$files, sapply(dat$renamed, `[[`, 2)))
  
  return( data_key[] )
}
