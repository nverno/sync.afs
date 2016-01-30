##' @include utils.R
NULL

##' Gather information about data files stored on afs.  Throws error if
##' there is no matching master file for R data.  This happens if 'rnames'
##' is defined and the corresponding value in 'files' can't be located.
##'
##' @param path Path to root directory to search for files.
##' @param files Files to gather info about (get with process_tracker)
##' @param fixed Match patterns exactly (surround with regexp anchors)
##' @param rnames R data names to match to files (if NULL then new names are made)
##' @return A data.table \itemize{
##'   \item size: size of file in kb
##'   \item modified: date last modified
##'   \item status_change: date of last change in file status
##'   \item accessed: date the file was last accessed
##'   \item lastmod: difference in days since last modification from current date.
##'   \item filetype: file extension
##'}
##' @importFrom tools file_path_sans_ext
##' @export
file_info <- function(path=getOption('afs.path'), files, fixed=TRUE, rnames=NULL) {
  files <- tolower(files)
  patts <- if (fixed) paste0('^', files, '$') else files
  if (is.null(rnames)) rnames <- NA_character_
  if (!is.character(rnames)) stop("'rnames' should be character type.")
  
  ## Get full file paths
  paths <- lapply(patts, function(i)
    list.files(path=path, pattern=i, full.names=TRUE, recursive = TRUE, 
      ignore.case = TRUE))
  
  ## Can't handle multiple matching files
  if ((inds <- any(lengths(paths) > 2L))) 
    stop(sprintf("Multiple matches for %s", paths[inds]))
  
  ## Error or warning for missing files
  missed <- which(!lengths(paths))
  if (length(missed)) {
    if (length((inds <- missed[!is.na(rnames[missed])]))) {
      err <- sprintf("Couldn't find master file(s) for %s",
                     paste(rnames[inds], collapse=', '))
      stop(err)
    } else
      warning(sprintf("Couldn't find %s", paste(files[missed], collapse=", ")))
  }    

  ## Remove missing
  paths <- nonEmpty(paths)
  
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
  docs <- tolower(basename(short))

  ## Add file/directory names
  lastmod <- filetype <- modified <- lastmod <- filename <- rname <-
    afs_path <- NULL
  finfo[, `:=`(directory = dirs, filename = docs)]

  ## Find time since modifications and file sizes
  finfo[, lastmod := as.POSIXlt(Sys.Date()) - modified]
  finfo[, filetype := tools::file_ext(short)]

  ## Add rnames: only create new ones where NA_character_
  ## new_names <- tolower(tools::file_path_sans_ext(files))
  finfo[data.table(filename=files, rname=rnames), rname := rname,
    on='filename', nomatch=0L]
  finfo[is.na(rname), rname := tolower(tools::file_path_sans_ext(filename))]
  
  ## Add AFS path from root
  finfo[, afs_path := short]
  
  ## Ordering
  ord <- c('rname', 'filename', 'modified', 'lastmod')
  data.table::setcolorder(finfo, c(ord, setdiff(names(finfo), ord)))
  data.table::setkeyv(finfo, cols=c('filename'))
  finfo[]
}

