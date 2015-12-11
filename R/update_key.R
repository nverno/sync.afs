##' @title Update the data_key with new entries/name changes
update_key <- function(tracker=file.path(get_afs(), "file_tracker.txt"), data_key=data_key) {
  dat <- process_tracker(tracker)
  if (length(dat$renamed)) {  # this is wrong
    data_key[filename %in% sapply(dat$renamed, `[[`, 1),
             filename := sapply(dat$renamed, `[[`, 2)]
  }

  ## new_files <- setdiff(dat$files, data_key[['filename']])
}


add_new_data <- function(new_files, path=get_afs()) {
  ## Get file info
  new_data <- file_info(files=new_files)
  if (!nrow(new_data)) return()

  ## Create the data key
}

library(data.table)
dat <- data.table(n1 = letters[1:10], n2 = letters[11:20])

## Rename values in `n2` using a key like this
## Replace the first values where they occur in `n2` with the second values
## so replace the "t" with "y" and the "l" with "z" in `n2`
renamed <- list(c("t", "y"),
                c("l", "z"))

dat[match(dat$n2, sapply(renamed, `[[`, 1), 0L),
    n2 := sapply(renamed, `[[`, 2)]

## Expected outcome


