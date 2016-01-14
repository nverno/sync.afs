##' @include utils.R
NULL

##' Read master data associated with R data file.
##' @title Read master data file corresponding to a R data filename
##' @importFrom haven read_sas
##' @import data.table
##' @import readxl
##' @param dname data name in R
##' @param path Path to root AFS directory
##' @param dkey data key to match R data files to master files (default data_key)
##' @param ... arguments to be passed to data reading function
##' @param data_key Key matching R data to master files.
##' @examples
##' \dontrun{
##'   pp_raw <- get_data('pp_raw')
##' }
##' @return data.table
##' @export
get_data <- function(dname, path=get_afs(), dkey=sync.afs::data_key, ...) {
  if (!exists("dkey"))
    stop('data_key isn\'t made, use the create_data_key_template() function.')
  if (!(dname %in% dkey[['rname']]))
    stop('That data is not named in the key.')
  rname <- filetype <- afs_path <- NULL
  m <- match.call(expand.dots=TRUE)[-1L]

  path <- file.path(path, dkey[rname == dname, afs_path])
  res <- if (requireNamespace('rio', quietly=TRUE)) {
    if (rio:::get_ext(path) == 'sas7bdat') {
        column.labels <- if ('column.labels' %in% names(m)) m[['column.labels']] else TRUE
        rio::import(path, column.labels=column.labels, ...)
    } else rio::import(path, ...)
  } else {
    ## choose how to read
    ftype = dkey[rname == dname, filetype]
    read <- switch(ftype,
                   'sas7bdat' = haven::read_sas,
                   'csv' = read.csv,
                   'txt' = read.table,
                   'xls' = ,
                     'xlsx' = readxl::read_excel,
                   fread)
    res <- read(path, ...)
    ## Use rio instead
  }
  data.table::setDT(res)[]
}
