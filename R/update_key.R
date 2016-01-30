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
  dat$files <- tolower(dat$files)
  rname <- filename <- NULL
  
  ## Renamed files
  ## Just update 'filename', 'rname' stays the same if defined
  if (length(dat$renamed)) {
    old_names <- unlist(lapply(dat$renamed, `[[`, 1), use.names=FALSE)
    new_names <- unlist(lapply(dat$renamed, `[[`, 2), use.names=FALSE)
    dkey[filename %in% old_names, filename := tolower(new_names)]
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

##' Update R file names associated with master files.
##' 
##' @param rnames New R data names
##' @param filenames Corresponding master file names.
##' @param key Data key to update (getOption('afs.key))
##' @export
set_key_names <- function(rnames, filenames, key=getOption('afs.key')) {
  dkey <- get_key()
  dkey[list(filename = filenames), rname := rnames, on='filename']
  p <- file.path(system.file('extdata', package='sync.afs'), getOption('afs.key'))
  data_key <- dkey
  save(data_key, file=p)
}
