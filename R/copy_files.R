##' Copy files from AFS given file extensions.
##' @title Copy files from AFS 
##' @param ext File extensions (character vector)
##' @param outdir Name of directory to copy files to
##' @param dkey Data key
##' @param path Path to root AFS node.
##' @param ... Passed to \code{file.copy}
##' @return TRUE if all files copied successfully.
##' @export
copy_files <- function(ext, outdir='temp', dkey=sync.afs::data_key, 
                       path=get_afs(), ...) {
    check_afs()
    filetype <- afs_path <- NULL
    filepaths <- dkey[filetype %in% ext, afs_path]
    dir.create(outdir)
    res <- file.copy(from = file.path(path, filepaths),
                     to = outdir, ...)
    if (!(any(res))) {
        warning(sprintf("Failed to copy %s", paste(basename(filepaths), collapse=",")))
        return( FALSE )
    }
    TRUE
}
