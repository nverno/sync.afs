.onLoad <- function(...) {
    ## Load user paths
    try(afs_read_opts(), silent = TRUE)
}
