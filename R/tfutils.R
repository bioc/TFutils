
#' utility to read FIMO outputs from local resource(cluster), assuming bed text split by chromosome
#' @importFrom IRanges IRanges
#' @importFrom GenomicRanges GRanges
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
  if (!requireNamespace("data.table")) stop("install data.table to use this function")
  stopifnot(length(tf)==1, is(tf, "character"))
  chromosome = chr
  myfile = system.file(paste0(tf,"/",chromosome,".bed"), package="TFutils")
  chrbed = data.table::fread(myfile)
  chrbed
  #chr = plyr::rename(chrbed, c("V1"= "chr","V2"="start","V3"="end","V4"="interval","V5"="score","V6"="strand","V7"="pvalue"))
}

#
##' acquire a GRanges representing binding sites for a TF
##' @importFrom IRanges IRanges
##' @importFrom methods is
##' @importFrom GenomicRanges GRanges
##' @importFrom S4Vectors "mcols<-"
##' @importFrom S4Vectors "mcols"
##' @param conv_name character(1) conventional name for TF
##' @param map list with elements consisting of the notation for TFs; names of list elements are the conv_names
##' @param which a GRanges instance limiting the request
##' @param meth a function with two parameters, tf (a character(1) string) and which (a GRanges instance) that retrieves TF 'bed'-like records from a resource
##' @note We assume that the retrieval yields a data.frame in the format
##' \code{
##'     V1    V2    V3               V4        V5 V6       V7
##'  chr1 11496 11511 chr1:11496-11511 -1.555560  - 5.92e-04
##'  chr1 11520 11535 chr1:11520-11535 -3.161620  + 9.12e-04
##' }
##' @examples
##' data(named_tf)
##' requireNamespace("GenomicRanges")
##' requireNamespace("IRanges")
##' getBS("VDR", named_tf, GenomicRanges::GRanges("chr1", IRanges::IRanges(1,25000)), 
##'      importFIMO_local)
#getBS = function( conv_name, map, which, meth ) {
#  stopifnot(!missing(which), is(which, "GRanges"))
#  stopifnot(is(meth, "function"))
#  mname = map[[conv_name]] 
#  text = meth( mname, which )
#  ans = GRanges(text$V1, IRanges(text$V2, text$V3), strand=text$V6)
#  mcols(ans) = text[,c("V5", "V7")]
#  names(mcols(ans)) = c("score", "p")
#  ans
#}
#
