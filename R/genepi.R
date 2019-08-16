#' demonstrate interoperation of TF catalog with GWAS catalog
#' @param traitTag character(1) string found in DISEASE/TRAIT field of gwascat instance
#' @param gwascat instance of \code{\link[gwascat]{gwaswloc-class}}
#' @return data.frame 
#' @examples
#' data(gwascat_hg19_chr17)
#' directHitsInCISBP("Prostate cancer" , gwascat_hg19_chr17)
#' @export
directHitsInCISBP = function(traitTag, gwascat) {
   reqNS("S4Vectors")
   reqNS("dplyr")
   stopifnot(traitTag %in% gwascat$`DISEASE/TRAIT`)
   as.data.frame(S4Vectors::mcols(gwascat)) %>%  # names are normalized
     dplyr::select(DISEASE.TRAIT, MAPPED_GENE) %>%
     dplyr::filter(DISEASE.TRAIT==traitTag) %>%
     dplyr::mutate(HGNC=MAPPED_GENE) %>%
     dplyr::inner_join(cisbpTFcat) %>%
     dplyr::select(HGNC, Family_Name) %>% unique()
   }
symToTFTName = function(TFsym, indToUse=1) {
  tfmap = TFutils::tftCollMap
  ind = which(tfmap$hgnc.heur == TFsym)
  if (length(ind)==0) stop(paste("could not find", TFsym, "in tftCollMap"))
  if (length(ind)>1) {
        warning(paste("found more than one heuristic match to",
             TFsym, "be sure indToUse is properly set"))
        message(paste("using", tfmap[ind[indToUse],1]))
        }   
  tfmap[ind[indToUse],1]
}
#' Use MSigDB TF targets resource to find targets of input TF and find traits to which these targets have been mapped
#' @importFrom GSEABase geneIds SymbolIdentifier mapIdentifiers geneIdType
#' @param TFsym character(1) symbol for a TF must be present in \code{tftCollMap[, "hgnc.heur"]}
#' @param gsc an instance of \code{\link[GSEABase]{GeneSetCollection-class}}, intended to enumerate
#' targets of a single transcription factor in each GeneSet, as in TFutils::tftColl
#' @param gwcat instance of \code{\link[gwascat]{gwaswloc-class}}
#' @param ntraits numeric(1) number of traits to report
#' @param force logical see note, set to true if you want to skip mapping from TFsym to a specific motif or TF identifier used as name of a GeneSet in gsc
#' @param \dots character() vector of fields in mcols(gwcat) to include
#' @note If \code{tftCollMap[, "hgnc.heur"]} does not possess the necessary
#' symbol, set force = TRUE to use a known 'motif' name among `names(gsc)`
#' @examples
#' suppressPackageStartupMessages({
#' library(GSEABase)
#' library(TFutils)
#' })  # more results if you substitute ebicat37 from gwascat below
#' topTraitsOfTargets("MTF1" , tftColl, gwascat_hg19_chr17)
#' @export
topTraitsOfTargets = function(TFsym, gsc, gwcat, ntraits=6, force=FALSE, ...) {
  if (geneIdType(gsc[[1]])@type != "Symbol") {
   message("remapping identifiers of input GeneSetCollection to Symbol...")
   gsc = mapIdentifiers(gsc, SymbolIdentifier("org.Hs.eg.db"))
   message("done")
   }   
  collToUse = symToTFTName(TFsym, ...)
  targs = intersect(geneIds(gsc[[collToUse]]), gwcat$MAPPED_GENE)
  intinds = which(gwcat$MAPPED_GENE %in% targs)

  ans = mcols(gwcat[intinds])[, c("DISEASE/TRAIT", "MAPPED_GENE",
       "SNPS", "CHR_ID", "CHR_POS", ...)]
  trtab = table(glis <- gwcat$`DISEASE/TRAIT`[which(gwcat$MAPPED_GENE %in% targs)])
  topl = sort(trtab, decreasing=TRUE)[seq_len(ntraits)]
  ans = ans[which(ans[,1] %in% names(topl)),]
  as.data.frame(ans) %>% arrange(DISEASE.TRAIT) %>% unique()
}

