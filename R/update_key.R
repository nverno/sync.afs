##' @title Update the data_key with new entries/name changes
##' @param path Path to base AFS directory from which to search (default to get_afs()).
##' @param tracker Name of the text file containing filenames to track (default to 'file_tracker.txt').
##' @param data_key The current data_key to update (default to data/data_key.rda).
update_key <- function(path=get_afs(), tracker="file_tracker.txt", data_key=data_key) {
  tracker <- file.path(path, tracker)
  dat <- process_tracker(tracker)
    
  ## New files
  inds <- which(!(dat$files %in% data_key[['filename']]))

  ## Renamed files
  if (length(dat$renamed)) {
    old_names <- unlist(lapply(dat$renamed, `[[`, 1), use.names=FALSE)
    new_names <- unlist(lapply(dat$renamed, `[[`, 2), use.names=FALSE)
    data_key[sapply]
  
  ## Update key with renamed files/new files
  if (length(dat$renamed) | length(inds)) {
    new_files <- c(dat$files[inds], unlist(lapply(dat$renamed, `[[`, 2), use.names = FALSE))
    finfo <- file_info(path, files=new_files)
    if (nrow(finfo)) {
      if (length(dat$renamed)) {
        
        rbind(copy(data_key))[finfo, rname := i.rname, on = 'filename'][]
        rename_inds <- data_key[match(oldnames, filename, 0L), ]
      }
    }
  }
}

## rbind(copy(dat)[new_dat, n3 := i.n1, on = "n2"], new_dat[!dat, on = 'n2'])












