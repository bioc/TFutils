#' create a list of GRanges for FIMO hits in a GenomicFiles instance, corresponding to a GRanges-based query
#' @importFrom Rsamtools seqinfo scanTabix
#' @importFrom GenomicFiles reduceByRange
#' @import SummarizedExperiment
#' @param gf GenomicFiles instance, like fimo16 in TFutils
#' @param query a GRanges specifying ranges to check for TF binding scores
#' @return a list of GRanges, produced by GenomicFiles::reduceByRange 
#' @note Be sure to use `register([BPPARAM])` appropriately.
#' @examples
#' if (interactive()) {   # need internet
#'  # setup -- annotate fimo16 object and create an informative
#'  # query
#'  colnames(fimo16) = fimo16$HGNC
#'  si = GenomeInfoDb::Seqinfo(genome="hg19")["chr17"] # to fix query genome
#'  myg = GRanges("chr17", IRanges(38.07e6,38.09e6), seqinfo=si)
#'  requireNamespace("BiocParallel")
#'  BiocParallel::register(BiocParallel::SerialParam())
#'  f1 = fimo_granges(fimo16[, c("VDR", "POU2F1")], myg)
#'  f1
#' }
#' @export
fimo_granges = function (gf, query) {
    SummarizedExperiment::rowRanges(gf) <- query
    proctext = function(x) {
        con = textConnection(x)
        on.exit(close(con))
        dtf = read.delim(con, h = FALSE, stringsAsFactors = FALSE, 
            sep = "\t")
        colnames(dtf) = c("chr", "start", "end", "rname", "score", 
            "dir", "pval")
        ans = with(dtf, GRanges(seqnames = chr, IRanges(start, 
            end), rname = rname, score = score, dir = dir, pval = pval))
        ans
    }
    nfun = function(ans) lapply(ans, lapply, proctext)
    ans = GenomicFiles::reduceByRange(gf, MAP = function(r, f) scanTabix(f, 
        param = r), REDUCE = nfun)
    if (ncol(gf) == 1) {  # REDUCE IS NOT INVOKED FOR ncol(fg) == 1
        ans = lapply(ans, function(z) proctext(z[[1]]))
        ans = lapply(ans, function(z) {
          seqinfo(z) = seqinfo(query)
          z
        })
       }
    else {
        ans = unlist(ans, recursive = FALSE)
        ans = lapply(ans, lapply, function(x) {
            seqinfo(x) = seqinfo(query)
            x
        })
    }
    names(ans) = rep(colnames(gf), length(query))
    ans
}

#fimo_granges_OLD = function (gf, query) 
#{
#    rowRanges(gf) = query
#    nfun = function(ans) lapply(ans, lapply, function(x) {
#        con = textConnection(x)
#        on.exit(close(con))
#        dtf = read.delim(con, h = FALSE, stringsAsFactors = FALSE, 
#            sep = "\t")
#        colnames(dtf) = c("chr", "start", "end", "rname", "score", 
#            "dir", "pval")
#        ans = with(dtf, GRanges(seqnames = chr, IRanges(start, 
#            end), rname = rname, score = score, dir = dir, pval = pval))
#        ans
#    })
#    ans = reduceByRange(gf, MAP = function(r, f) scanTabix(f, 
#        param = r), REDUCE = nfun)
#    ans = unlist(ans, recursive=FALSE)
#    names(ans) = rep(colnames(gf), length(query))
#    lapply(ans, lapply, function(x) {seqinfo(x) = seqinfo(query); x})
#}
