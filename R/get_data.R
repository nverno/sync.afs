##' @include utils.R
NULL

##' load the data key onload
##' work around saving persitent data
##' @keywords internal
load_key <- function(data=getOption('afs.key')) {
  p <- file.path(system.file('extdata', package='sync.afs'), paste0(data, '.rda'))
  load(p, envir=getNamespace('sync.afs'))
}

##' Get the data key
##' @param key key name (getOption('afs.key'))
##' @export
get_key <- function(key=getOption('afs.key')) { 
  getFromNamespace(key, 'sync.afs') 
}

##' Read master data associated with R data file.
##'
##' @importFrom haven read_sas
##' @import data.table
##' @importFrom readxl read_excel
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
get_data <- function(dname, path=getOption('afs.path'), 
                     dkey=get_key(), ...) {
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
                   data.table::fread)
    res <- read(path, ...)
    ## Use rio instead
  }
  data.table::setDT(res)[]
}
