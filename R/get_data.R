##' @include utils.R
NULL

##' Read master data associated with R data file.
##' @title Read master data file corresponding to a R data filename
##' @importFrom haven read_sas
##' @import data.table
##' @param dname data name in R
##' @param dkey data key to match R data files to master files (default data_key)
##' @param ... arguments to be passed to data reading function
##' @param data_key Key matching R data to master files.
##' @examples
##' \dontrun{
##'   pp_raw <- get_data('pp_raw')
##' }
##' @return corresponding master file (prior to cleaning)
##' @export
get_data <- function(dname, dkey=data_key, ...) {
  if (!exists("dkey"))
    stop('data_key isn\'t made, use the create_data_key_template() function.')
  if (!(dname %in% dkey[['rname']]))
    stop('That data is not named in the key.')
  rname <- filetype <- afs_path <- NULL
  
  ## choose how to read
  ftype = dkey[rname == dname, filetype]
  read <- switch(ftype,
                 'sas7bdat' = haven::read_sas,
                 'csv' = read.csv,
                 'txt' = read.table,
                 read.table)
  path <- file.path(get_afs(), dkey[rname == dname, afs_path])
  setDT(read(path, ...))[]
}

