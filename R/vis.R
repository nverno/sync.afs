##' @include utils.R
NULL

##' @title directory_vis
##' @details
##' Makes an igraph visualization of the file-directory structure.
##' 
##' @param finfo data.table(data.frame) returned from \code{data_info}
##' @param add_root logical whether to add root node to join all directories (default to TRUE).
##' @param ... Passed to plot.igraph
##' @return invisibly returns the graph.
##' @import igraph
##' @export
directory_vis <- function(finfo, add_root=TRUE, ...) {
    es <- data.frame(dirs = finfo$directory,
                     docs = finfo$rname,
                     weight = finfo$size,
                     mod = finfo$lastmod, stringsAsFactors = FALSE)

    ## Get an idea of the layout
    g <- igraph::graph_from_data_frame(es[, c("dirs", "docs", "weight")])

    if (add_root) {
        ## Add a root node 'Lixi' (all these are subdirectories presumably)
        g <- g + igraph::vertex('root') + igraph::edges(c(rbind('root', unique(es$dirs))))
    }
    igraph::V(g)$name <- sub('\\..*', '', igraph::V(g)$name)
    igraph::V(g)$color <- 'tan'
    plot(g, ...)
    invisible(g)
}

##' @title data_vis
##' @import igraph
##' @import data.table
##' @param tracker Path to tracker file on AFS (has default)
##' @param path Path to root AFS folder (has default)
##' @param add_root Logical to add root to graph
##' @param ... passed to plot.igraph
##' @examples
##' \dontrun{
##'
##'    ## Defaults to Lixi's root directory and tracked files.
##'    data_vis()
##'
##' }
##' @export
data_vis <- function(tracker=file.path(get_afs(), "file_tracker.txt"),
                     path=get_afs(), add_root=TRUE, ...) {
  dat <- process_tracker(tracker)
  finfo <- file_info(path, files=dat$files)
  directory_vis(finfo, add_root, ...)
}

