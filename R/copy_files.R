##' Copy files from AFS given file extensions.
##' @title Copy files from AFS 
##' @param ext File extensions (character vector)
##' @param afs An AFS object with path set to user root directory
##' @param outdir Name of directory to copy files to
##' @param dkey Data key
##' @param ... Passed to \code{file.copy}
##' @return TRUE if all files copied successfully.
##' @export
copy_files <- function(ext, afs, outdir='temp', dkey=sync.afs::data_key, ...) {
    if (!afs$connected()) afs$signin()
    if (is.null(afs$path)) stop('AFS path is not defined.')
    filetype <- afs_path <- NULL
    filepaths <- dkey[filetype %in% ext, afs_path]
    dir.create(outdir)
    res <- file.copy(from = file.path(afs$path, filepaths),
                     to = outdir, ...)
    if (!(any(res))) {
        warning(sprintf("Failed to copy %s", paste(basename(filepaths), collapse=",")))
        return( FALSE )
    }
    TRUE
}
