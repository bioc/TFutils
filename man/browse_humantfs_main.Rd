% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambert_ccbr_get.R
\name{browse_humantfs_main}
\alias{browse_humantfs_main}
\title{use DT::datatable to browse the Lambert table S1}
\usage{
browse_humantfs_main(cache = BiocFileCache::BiocFileCache(ask = FALSE))
}
\arguments{
\item{cache}{a BiocFileCache instance}
}
\value{
result of DT::datatable
}
\description{
use DT::datatable to browse the Lambert table S1
}
\note{
PMIDs are converted to HTML anchors and DT::datatable is run with \code{escape=FALSE}.
}
\examples{
if (interactive()) browse_lambert_main()
}
