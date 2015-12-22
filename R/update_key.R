##' Update data_key by processing the tracking document and renaming/adding
##' files to data_key.  Throws error if there is an R data file associated with
##' a master file that can't be found.
##'
##' @title Update the data_key with new entries/name changes
##' @param path Path to base AFS directory from which to search (default to get_afs()).
##' @param tracker Name of the text file containing filenames to track (default to 'file_tracker.txt').
##' @param data_key The current data_key to update (default to data/data_key.rda).
##' @param save_key Save the key in the data directory?  Use this if going to push update.
##' @param src_path Path to package source, only used if save_key is TRUE.
##' @import data.table
##' @export
update_key <- function(path=get_afs(), tracker="file_tracker.txt", data_key=data_key,
                       save_key=FALSE, src_path='~/work/sync.afs') {
  tracker <- file.path(path, tracker)
  dat <- process_tracker(tracker)
  rname <- filename <- NULL
  
  ## New files
  new_files <- dat$files[(!(dat$files %in% data_key[['filename']]))]

  ## Renamed files
  ## Just update 'filename', change 'rname' back after
  if (length(dat$renamed)) {
    old_names <- unlist(lapply(dat$renamed, `[[`, 1), use.names=FALSE)
    old_rnames <- data_key[old_names, rname]
    new_names <- unlist(lapply(dat$renamed, `[[`, 2), use.names=FALSE)
    data_key[old_names, filename := new_names]
  }

  ## Get file info -- including new files/renamed
  files <- c(new_files, data_key$filename)
  finfo <- file_info(files=files)

  ## Rename 'rname'
  if (length(dat$renamed)) {
    missing_rnames <- old_rnames[!(new_names %in% finfo$filename)]
    finfo[new_names, rname := old_rnames]
    if (length(missing_rnames))
      stop(sprintf('Couldn\'t find the master file for %s.',
                   paste0(missing_rnames, collapse=', ')))
  }

  ## Save
  if (save_key) {
    data_key <- finfo
    save(data_key, file = file.path(src_path, 'data', 'data_key.rda'), compress='xz')
  }
  finfo[]
}

