setGeneric("importFIMO", function(src, parms, ...) standardGeneric("importFIMO"))

#' import a FIMO bed-like file
#' # @importFrom utils read.delim
#' @rdname importFIMO
#' @aliases importFIMO,TabixFile,GRanges-method importFIMO
#' @param src TabixFile instance
#' @param parms a GRanges instance delimiting the import; multiple GRanges can be used
#' @param \dots passed to GenomicRanges::GRanges
#' @return instance of GRanges
#' @examples
#' if (requireNamespace("Rsamtools")) {
#'  tf = Rsamtools::TabixFile(system.file("M5946_1/chr1.bed.gz", package="TFutils"))
#'  importFIMO(tf, GenomicRanges::GRanges("chr1", IRanges::IRanges(1e6,11e6)))
#'  }
#' @export
setMethod("importFIMO", c("TabixFile", "GRanges"), function(src, parms, ...) {
  jnk = lapply(c("GenomeInfoDb", "Rsamtools", "GenomicRanges", "IRanges",
    "GenomeInfoDb"), reqNS)
  tmp = Rsamtools::scanTabix(src, param=parms) # list with one element per range in parms
  dfs = lapply(tmp, function(x) utils::read.delim(textConnection(x), header=FALSE))
  alldf = do.call(rbind, dfs)
  GenomicRanges::GRanges(alldf$V1, IRanges::IRanges(
      start=alldf$V2, end=alldf$V3), score=alldf$V5, pvalue=alldf$V7, strand=alldf$V6, 
      seqinfo=GenomeInfoDb::seqinfo(parms), ...)
})

#' @rdname importFIMO
#' @aliases importFIMO,character,missing-method
#' @export
setMethod("importFIMO", c("character", "missing"), function(src, parms, ...) {
  if (!requireNamespace("data.table")) stop("install data.table to use this function")
  stopifnot(file.exists(src)) # assume textual bed-like file
  tmp = data.table::fread(src)
  GenomicRanges::GRanges(tmp$V1, IRanges::IRanges(
      start=tmp$V2, end=tmp$V3), score=tmp$V5, pvalue=tmp$V7, strand=tmp$V6, 
      ...)
})

