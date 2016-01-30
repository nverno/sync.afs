##' @include utils.R
NULL

##' Makes an igraph visualization of the file-directory structure.
##' @title directory_vis
##' @param finfo data.table(data.frame) returned from \code{data_info}
##' @param add_root logical whether to add root node to join all directories (default to TRUE).
##' @param ... Passed to plot.igraph
##' @return invisibly returns the graph.
##' @export
directory_vis <- function(finfo, add_root=TRUE, ...) {
    es <- data.frame(dirs = finfo$directory,
                     docs = finfo$rname,
                     weight = finfo$size,
                     mod = finfo$lastmod, stringsAsFactors = FALSE)
    if (!requireNamespace(igraph, quietly = TRUE)) return()
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

##' Wrapper to visualize the location of files in the AFS directory.
##' Processes a tracking document to get files prior to passing to directory_vis.
##' @param afs An AFS object with path set to user root
##' @param tracker Path to tracker file (default getOption('afs.tracker'))
##' @param add_root Logical to add root to graph
##' @param ... passed to plot.igraph
##' @param path Path to root AFS folder (has default)
##' @examples
##' \dontrun{
##'
##'    ## Defaults to Lixi's root directory and tracked files.
##'    data_vis()
##'
##' }
##' @return Returns the igraph invisibly.
##' @export
data_vis <- function(afs, tracker=getOption('afs.tracker'), add_root=TRUE, ...) {
  if (!requireNamespace(igraph, quietly=TRUE)) return()
  dat <- process_tracker(tracker)
  finfo <- file_info(path, files=dat$files)
  directory_vis(finfo, add_root, ...)
}

