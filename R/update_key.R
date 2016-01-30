##' Update data_key by processing the tracking document and renaming/adding
##' files to data_key.  
##'
##' @param path Path to AFS root directory (getOption('afs.path))
##' @param tracker tracking file (getOption('afs.tracker'))
##' @param data Data key (getOption('afs.key')
##' @param save_key Save the returned key (default to "pkg/extdata/data_key.rda")
##' @param outfile Where to save key, only used if save_key is TRUE.
##' @export
update_key <- function(path=getOption('afs.path'), 
                       tracker=getOption('afs.tracker'), 
                       data=get_key(),
                       save_key=TRUE, outfile=NULL) {
  dkey <- data.table::copy(data)
  dat <- process_tracker(tracker=tracker)
  rname <- filename <- NULL
  
  ## Renamed files
  ## Just update 'filename', 'rname' stays the same if defined
  if (length(dat$renamed)) {
    old_names <- unlist(lapply(dat$renamed, `[[`, 1), use.names=FALSE)
    new_names <- unlist(lapply(dat$renamed, `[[`, 2), use.names=FALSE)
    dkey[old_names, filename := new_names]
  }

  ## New files
  new_files <- dat$files[(!(dat$files %in% dkey[['filename']]))]

  ## Get file info -- including new files/renamed
  files <- c(dkey$filename, new_files)
  rnames <- dkey[data.table(filename=files), rname, on='filename']
  finfo <- file_info(path=path, files=files, rnames=rnames)

  ## Save
  if (save_key) {
    data_key <- finfo
    if (is.null(outfile)) {
      save(data_key, system.file('extdata', package='sync.afs'))
    }
    save(data_key, file=outfile)
  }
  invisible(finfo[])
}

