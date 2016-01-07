##' Copy files from AFS given file extensions.
##' @title Copy files from AFS 
##' @param ext File extensions (character vector)
##' @param outdir Name of directory to copy files to
##' @param ... Passed to \code{file.copy}
##' @return TRUE if all files copied successfully.
##' @export
copy_files <- function(ext, outdir='temp', ...) {
    check_afs()
    filetype <- afs_path <- data_key <- NULL
    filepaths <- data_key[filetype %in% ext, afs_path]
    dir.create(outdir)
    res <- file.copy(from = file.path(get_afs(), filepaths),
                     to = outdir, ...)
    if (!(any(res))) {
        warning(sprintf("Failed to copy %s", paste(basename(filepaths), collapse=",")))
        return( FALSE )
    }
    TRUE
}
