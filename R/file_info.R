##' @title file_info
##' @details
##' Gather information about data files stored on afs.
##' 
##' @param path AFS path to root directory (defaults to Lixi's folder)
##' @param files Files to gather info about (get with process_tracker)
##' @return A data.table \itemize{
##'   \item size: size of file in kb
##'   \item modified: date last modified
##'   \item status_change: date of last change in file status
##'   \item accessed: date the file was last accessed
##'   \item lastmod: difference in days since last modification from current date.
##'   \item filetype: file extension
##'}
##' @import data.table
file_info <- function(path=if(Sys.info()[['sysname']]=='Linux') afs.linux else afs, files) {
    ## Get full file paths
    paths <- lapply(files, function(i)
        list.files(path=path, pattern=i, full.names=TRUE, recursive = TRUE))

    ## File info
    finfo <- data.table::rbindlist(lapply(paths, function(f) {
        info <- file.info(f)
        list(
            'size' = info[["size"]]/1024,
            'modified' = info[['mtime']],
            'status_change' = info[['ctime']],
            'accessed' = info[['atime']]
        )
    }))

    ## Drop the afs prefix
    short <- sub(paste0(path, "/"), '', paths, fixed=TRUE)
    dirs <- dirname(short)
    docs <- basename(short)

    ## Add file/directory names
    finfo[, `:=`(directory = dirs, doc = docs)]

    ## Find time since modifications and file sizes
    finfo[, lastmod := as.POSIXlt(Sys.Date()) - modified]
    finfo[, filetype := tools::file_ext(short)]
    finfo[]
}

