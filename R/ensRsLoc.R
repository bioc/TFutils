#' utility to obtain location etc. for rsids of SNPs
#' @param rsids character vector of dbSNP identifiers
#' @note Uses rest.ensembl.org, posting to variant_recorder/homo_sapiens.
#' Parses result minimally, using only the first SPDI to obtain
#' location information, adding 1 as ensembl genomic coordinates
#' are zero-based.
#' @examples
#' get_rslocs_38()
#' @export
get_rslocs_38 = function(rsids = c("rs6060535", "rs56116432")) {
 server <- "https://rest.ensembl.org"
 ext <- "/variant_recoder/homo_sapiens"
 r <- httr::POST(paste(server, ext, sep = ""), 
       httr::content_type("application/json"), 
       httr::accept("application/json"), 
       body = list(ids=rsids), encode="json")
 httr::stop_for_status(r)
 ans = rjson::fromJSON( rjson::toJSON( httr::content(r)))
 ids = lapply(ans, "[[", "id")
 spd = sapply(lapply(ans, "[[", "spdi"), "[", 1)
 sspd = strsplit(spd, ":")
 chrs = sapply(sspd, "[", 1)
 locs = as.numeric(sapply(sspd, "[", 2))+1
 ans = GenomicRanges::GRanges(chrs, IRanges::IRanges(locs, width=1))
 GenomeInfoDb::genome(ans) = "GRCh38"
 names(ans) = ids
 ans
}
