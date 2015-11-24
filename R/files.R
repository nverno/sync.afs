## AFS directory
afs <- "\\\\AFS\\.northstar.dartmouth.edu\\users\\d\\drp\\MOOSHUBB\\longterm\\lixi kong"

## Files to track
datafiles <- c("abba15ex.sas7bdat", "birchmash.sas7bdat", "birchmasv.sas7bdat", 
               "demomas11.sas7bdat", "elevmas12.sas7bdat", "firmash.sas7bdat", 
               "firmasv.sas7bdat", "hhcht15mas.sas7bdat", "hhtr15ht.sas7bdat", 
               "ppsubmas10c.sas7bdat", "pptreemas10.sas7bdat", "pptreemas10bv.sas7bdat", 
               "prismmas12.sas7bdat", "seesapmas11.sas7bdat", "soilmas11.sas7bdat", 
               "trsaptrmas11.sas7bdat", "trsaptrmas11bv.sas7bdat", "trseedmas11.sas7bdat", 
               "trspcovmas11.sas7bdat", "trsubmas11c.sas7bdat")
docfiles <- c("Data collected in 2015.docx",
              "Demonstration Plot Data History 1987~2011.docx", 
              "Documentation_Lixi.docx",
              "ELEV data history.docx",
              "GPS data history.docx", 
              "Permanent Data History 1986-2011.docx",
              "Segment Data History 1988-2012.docx", 
              "Soil Data History 86~11.docx",
              "Transect Data History 1987-2011.docx")

##' @title data_info
##' @details
##'  \describe{
##'    Gather information about data files stored on afs.
##' }
##' @param path AFS path to root directory (defaults to Lixi's folder)
##' @param files Files to gather info about (defaults to 'datafiles' and 'docfiles')
##' @return A data.table \itemize{
##'   \item size: size of file in kb
##'   \item modified: date last modified
##'   \item status_change: date of last change in file status
##'   \item accessed: date the file was last accessed
##'   \item lastmod: difference in days since last modification from current date.
##'   \item filetype: file extension
##'}
data_info <- function(path=afs, files=c(datafiles, docfiles)) {
    ## Get full file paths
    paths <- lapply(c(datafiles, docfiles), function(i)
        list.files(path=afs, pattern=i, full.names=TRUE, recursive = TRUE))

    ## File info
    finfo <- data.table::rbindlist(lapply(paths, function(f) {
        info <- file.info(f)
        list(
            'size' = info[["size"]]/1024,
            'modified' = info[['mtime']],
            'status_change' = info[['ctime']],
            'accessed' = info[['atime']]
        )
    }))

    ## Drop the afs prefix
    short <- sub(paste0(afs, "/"), '', paths, fixed=TRUE)
    dirs <- dirname(short)
    docs <- basename(short)

    ## Add file/directory names
    finfo[, ':='(directory = dirs, doc = docs)]

    ## Find time since modifications and file sizes
    finfo[, lastmod := as.POSIXlt(Sys.Date()) - modified]
    finfo[, filetype := tools::file_ext(short)]
    finfo[]
}
##' \description{
##'  Makes an igraph visualization of the file-directory structure.
##' }
##' @title directory_vis
##' @param finfo data.table(data.frame) returned from \code{data_info}
##' @param add_root logical whether to add root node to join all directories (default to TRUE).
##' @param ... Passed to plot.igraph
##' @return invisibly returns the graph.
directory_vis <- function(finfo, add_root=TRUE, ...) {
    stopifnot(require(igraph))
    edges <- data.frame(dirs = finfo$directory,
                        docs = finfo$doc,
                        weight = finfo$size,
                        mod = finfo$lastmod, stringsAsFactors = FALSE)

    ## Get an idea of the layout
    g <- graph_from_data_frame(edges[, c("dirs", "docs", "weight")])

    if (add_root) {
        ## Add a root node 'Lixi' (all these are subdirectories presumably)
        g <- g + vertex('Lixi') + edges(c(rbind('Lixi', dirs)))
    }
    V(g)$name <- sub('\\..*', '', V(g)$name)
    V(g)$color <- 'tan'
    plot(g, ...)
    invisible(g)
}

