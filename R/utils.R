##' Return the afs path for linux or on windows
##' @title get_afs
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
