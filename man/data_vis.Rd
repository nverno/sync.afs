% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis.R
\name{data_vis}
\alias{data_vis}
\title{Wrapper to visualize the location of files in the AFS directory.
Processes a tracking document to get files prior to passing to directory_vis.}
\usage{
data_vis(path = getOption("afs.path"), tracker = getOption("afs.tracker"),
  add_root = TRUE, ...)
}
\arguments{
\item{path}{Path to AFS root folder (getOption('afs.path'))}

\item{tracker}{Path to tracker file (getOption('afs.tracker'))}

\item{add_root}{Logical to add root to graph}

\item{...}{passed to plot.igraph}
}
\value{
Returns the igraph invisibly.
}
\description{
Wrapper to visualize the location of files in the AFS directory.
Processes a tracking document to get files prior to passing to directory_vis.
}
\examples{
\dontrun{

   ## Defaults to root directory and shows tracked files.
   data_vis()

}
}

