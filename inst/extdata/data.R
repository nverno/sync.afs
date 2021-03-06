##' Key matching R data names with SAS data master files.  Contains
##' information pertaining to data files and their documentation.  This
##' dataset can be updated using the internal update_data function.
##' @format A data table with 10 columns and varying number of rows.
##' \itemize{
##'   \item rname: Name of associated R data file.
##'   \item filename: Name of master file (subject to change).
##'   \item modified: Date of last modification.
##'   \item lastmod: Number of days since last modification (difftime).
##'   \item size: Size of file (in kb).
##'   \item status_change: Date of last change in file status.
##'   \item accessed: Date the file was last accessed.
##'   \item directory: Name of directory containing file.
##'   \item filetype: File extension.
##'   \item afs_path: Path to file starting from 'Lixi Kong' directory on AFS.
##' }
"data_key"



