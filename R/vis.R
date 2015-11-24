##' @details
##' Makes an igraph visualization of the file-directory structure.
##' 
##' @title directory_vis
##' @param finfo data.table(data.frame) returned from \code{data_info}
##' @param add_root logical whether to add root node to join all directories (default to TRUE).
##' @param ... Passed to plot.igraph
##' @return invisibly returns the graph.
##' @import igraph
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

