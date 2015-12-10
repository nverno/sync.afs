##' @include utils.R
NULL

##' @title Read master data file corresponding to a R data filename
##' @import haven
##' @import data.table
##' @param dname data name in R
##' @param ... arguments to be passed to reading function
##' @examples
##' \dontrun{
##'   pp_raw <- get_data('pp_raw')
##' }
##' @return corresponding master file (prior to cleaning)
##' @author Noah Peart
get_data <- function(dname, ...) {
  if (!exists("data_key"))
    stop('data_key isn\'t made, use the create_data_key_template() function.')
  if (!(dname %in% data_key[['rname']]))
    stop('That data is not named in the key.')
  
  ## choose how to read
  ftype = data_key[rname == dname, filetype]
  read <- switch(ftype,
                 'sas7bdat' = read_sas,
                 'csv' = read.csv,
                 'txt' = read.table,
                 read.table)
  path <- file.path(get_afs(), data_key[rname == dname, afs_path])
  read(path, ...)
}

