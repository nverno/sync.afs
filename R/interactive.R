##' @include utils.R
NULL

data_tree <- function(tracker=file.path(get_afs(), "file_tracker.txt"),
                      path=get_afs(), add_root=TRUE, ...) {
  dat <- process_tracker(tracker)
  finfo <- file_info(path, files=dat$files)
  directory_vis(finfo, add_root, ...)
