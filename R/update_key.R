##' Update data_key by processing the tracking document and renaming/adding
##' files to data_key.  
##'
##' @title Update the data_key with new entries/name changes
##' @param path Path to base AFS directory from which to search (default to get_afs()).
##' @param tracker Name of the text file containing filenames to track (default to 'file_tracker.txt').
##' @param data The current data key to update (default to data/data_key.rda).
##' @param save_key Save the key in the data directory?  Use this if going to push update.
##' @param src_path Path to package source, only used if save_key is TRUE.
##' @import data.table
##' @export
update_key <- function(path=get_afs(), tracker="file_tracker.txt", data=data_key,
                       save_key=FALSE, src_path='~/work/sync.afs') {
  dkey <- data.table::copy(data)
  tracker <- file.path(path, tracker)
  dat <- process_tracker(tracker)
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
  finfo <- file_info(files=files, rnames=rnames)

  ## Save
  if (save_key) {
    data_key <- finfo
    save(data_key, file = file.path(src_path, 'data', 'data_key.rda'), compress='xz')
  }
  finfo[]
}

