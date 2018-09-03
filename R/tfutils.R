
#' utility to read FIMO outputs from local resource(cluster), assuming bed text split by chromosome
#' @param tf character(1) file id
#' @param chr character(1) chromosome name
#' @return data.table instance
#' @examples
#' requireNamespace("GenomicRanges")
#' requireNamespace("IRanges")
#  requireNamespace("DT")
#' importFIMO_local_split("M5946_1", "chr1")
#' dim(importFIMO_local_split("M5946_1", "chr17"))
#' @export
importFIMO_local_split = function( tf, chr ) {
  reqNS("IRanges")
  reqNS("GenomicRanges")
  if (!requireNamespace("data.table")) stop("install data.table to use this function")
  stopifnot(length(tf)==1, is(tf, "character"))
  chromosome = chr
  myfile = system.file(paste0(tf,"/",chromosome,".bed"), package="TFutils")
  chrbed = data.table::fread(myfile)
  chrbed
  #chr = plyr::rename(chrbed, c("V1"= "chr","V2"="start","V3"="end","V4"="interval","V5"="score","V6"="strand","V7"="pvalue"))
}

