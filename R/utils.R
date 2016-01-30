##' Return the afs path for linux or on windows
##' @title Get AFS path
##' @export
get_afs <- function() {
  if (Sys.info()[['sysname']]=='Linux')
    "/afs/northstar.dartmouth.edu/users/d/drp/MOOSHUBB/longterm/lixi kong"
  else
    "\\\\AFS\\.northstar.dartmouth.edu\\users\\d\\drp\\MOOSHUBB\\longterm\\lixi kong"
}

##' Remove empty/null values from list
##' @title remove nulls/empty values from list
##' @param lst list
##' @return list w/o nulls/empty values
##' @export
nonEmpty <- function(lst) lst[sapply(lst, function(i) !is.null(i) && length(i))]

##' Load data.  If can't connect to data, returns and empty data.table.
##' @title Load data
##' @param data name of dataset
##' @param uppercase Set the names all to uppercase.
##' @return data.table or errors if can't reach AFS
##' @export
afs_load_data <- function(data, uppercase=TRUE) {
  if (!afs_yes()) {
    stop("No AFS tokens, can't load data from AFS.")
  }
  dat <- sync.afs::get_data(data, sync.afs::get_afs(), dkey)
  if (uppercase) 
    data.table::setnames(dat, names(dat), toupper(names(dat)))
  dat
}
