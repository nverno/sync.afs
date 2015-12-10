## Create data_key (as data.table) to store files/locations/info for quicker access
##' @title Create data_key
##' @param path path to afs directory
##' @importFrom tools file_ext file_path_sans_ext
##' @import data.table
##' @examples
##' \dontrun{
##'   data_key <- create_data_key_template()
##' }
##' @return data.table
##' @keywords internal
create_data_key_template <- function(path=sync.afs::get_afs()) {
  dat <- sync.afs::process_tracker()

  ## Get file info
  data_key <- file_info(files = dat$files)

  ## Create the data_key
  data_key[, `:=`(rname = tolower(tools::file_path_sans_ext(filename)),
                  afs_path = sub(paste0(path, "/*"), '', paths))]

  ## Ordering
  ord <- c('rname', 'filename', 'modified', 'lastmod')
  data.table::setcolorder(data_key, c(ord, setdiff(names(data_key), ord)))
  
  return( data_key[] )
}


