##' @include utils.R
NULL

##' @details
##' Makes an igraph visualization of the file-directory structure.
##' 
##' @title directory_vis
##' @param finfo data.table(data.frame) returned from \code{data_info}
##' @param add_root logical whether to add root node to join all directories (default to TRUE).
##' @param ... Passed to plot.igraph
##' @return invisibly returns the graph.
##' @import igraph
##' @export
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
        g <- g + vertex('Lixi') + edges(c(rbind('Lixi', unique(edges$dirs))))
    }
    V(g)$name <- sub('\\..*', '', V(g)$name)
    V(g)$color <- 'tan'
    plot(g, ...)
    invisible(g)
}

##' @import igraph
##' @import data.table
##' @param tracker Path to tracker file on AFS (has default)
##' @param path Path to root AFS folder (has default)
##' @param add_root Logical to add root to graph
##' @param ... passed to plot.igraph
##' @export
data_vis <- function(tracker=file.path(get_afs(), "file_tracker.txt"),
                     path=get_afs(), add_root=TRUE, ...) {
  dat <- process_tracker(tracker)
  finfo <- file_info(path, files=dat$files)
  directory_vis(finfo, add_root, ...)
}

