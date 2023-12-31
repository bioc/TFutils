% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensRsLoc.R
\name{get_rslocs_38}
\alias{get_rslocs_38}
\title{utility to obtain location etc. for rsids of SNPs}
\usage{
get_rslocs_38(rsids = c("rs6060535", "rs56116432"))
}
\arguments{
\item{rsids}{character vector of dbSNP identifiers}
}
\value{
GRanges instance
}
\description{
utility to obtain location etc. for rsids of SNPs
}
\note{
Uses rest.ensembl.org, posting to variant_recorder/homo_sapiens.
Parses result minimally, using only the first SPDI to obtain
location information, adding 1 as ensembl genomic coordinates
are zero-based.
}
\examples{
if (interactive()) get_rslocs_38() # see https://stat.ethz.ch/pipermail/bioc-devel/2020-October/017263.html
}
