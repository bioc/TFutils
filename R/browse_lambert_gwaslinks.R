#' browse several hundred disease-TF associations with hyperlinked PMIDs
#' @importFrom stats family
#' @importFrom utils read.csv read.delim
#' @examples
#' if (interactive()) browse_lambert_gwaslinks()
#' @return DT::datatable
#' @note Based on supplemental table S4 of PMID 29425488
#' @export
browse_lambert_gwaslinks = function() {
 dis = read.csv(system.file("lambert/S4_DiseaseTFMuts.csv", package=
   "TFutils"), 
   stringsAsFactors=FALSE, check.names=FALSE)
 dis[192,][,3] = gsub(", ", ",", dis[192,3])
 fabricate_anchor = function(x)
    sprintf("<a href=http://pubmed.gov/%s>%s</a>", x, x)
 pms = dis[,3]
 spms = strsplit(pms, ",")
 p0 = function(x) paste0(x, collapse=", ")
 dis$PMID = sapply(spms, function(z)p0(unname(sapply(z, function(x)fabricate_anchor(x)))))
 DT::datatable(dis, escape=FALSE)
}
