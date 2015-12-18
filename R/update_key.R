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

rbind(copy(dat)[new_dat, n3 := i.n1, on = "n2"], new_dat[!dat, on = 'n2'])


vct_numbers <- c(1,2,3,4)
vct_colours <- c("blue", "green", "yellow")
lst_coll <- list(vct_numbers = vct_numbers, vct_colours  = vct_colours)

library(data.table)

## Example data, a keyed data.table
dat <- data.table(c1=1:10, c2=letters[1:10], key='c2')

## Match at some indices (keyed column so should be binary search?)
new_dat <- c('d', 'j')

## This doesn't feel right -- I don't think this is taking advantage of the
## data.table ordering at all
dat[match(new_dat, c2, 0L), ]  # only want the index of the matches
#    c1 c2
# 1:  4  d
# 2: 10  j

## So, looking for this result,
## but this is just doing ordinary linear search (I say w/o actually looking at the code)
match(new_dat, dat[['c2']], 0L)
# [1]  4 10

dat[, ind := 1:.N][match(new_dat, c2, 0L), ind]

dat[new_dat, which=TRUE]

dat <- data.table(c1 = 1:1e7, c2 = intToUtf8(1:1e7))
