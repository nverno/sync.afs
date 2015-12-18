##' @title Update the data_key with new entries/name changes
update_key <- function(path=get_afs(), tracker="file_tracker.txt", data_key=data_key) {
  tracker <- file.path(path, tracker)
  dat <- process_tracker(tracker)

  ## New files
  inds <- which(!(dat$files %in% data_key[['filename']]))

  ## Renamed files
  
  ## Update key with renamed files/new files
  if (length(dat$renamed) | length(inds)) {
    new_files <- c(dat$files[inds], unlist(lapply(dat$renamed, `[[`, 2), use.names = FALSE))
    finfo <- file_info(path, files=new_files)
    if (nrow(finfo)) {
      if (length(dat$renamed)) {
        cols <- setdiff(names(data_key), 'rname')  # don't update names of R datafiles
        oldnames <- sapply(dat$renamed, `[[`, 1)
        rename_inds <- data_key[match(oldnames, filename, 0L), ]
      }
    }
  }
}

## rbind(copy(dat)[new_dat, n3 := i.n1, on = "n2"], new_dat[!dat, on = 'n2'])
