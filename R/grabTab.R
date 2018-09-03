#' create table of TF targets and related metadata
#' @import dplyr
#' @import magrittr
#' @importFrom methods as is
#' @param tfstub character(1) gene-like symbol for TF; will be grepped in names(gscoll)
#' @param gscoll a GSEABase GeneSetCollection
#' @param orgdb an instance of OrgDb as defined in AnnotationDbi
#' @param gwrngs a GRanges representing EBI gwascat, must have `DISEASE/TRAIT`, `MAPPED_GENE`
#' @return data.frame instance
#' @note This function will link together information on targets of a
#' given TF to the GWAS catalog.
#' @examples
#' gt = grabTab("VDR", gscoll=TFutils::tftColl,
#'    orgdb=org.Hs.eg.db::org.Hs.eg.db, gwrngs=TFutils::gwascat_hg19_chr17)
#' dim(gt)
#' head(gt)
#' @export
grabTab = function(tfstub="STAT1", gscoll=TFutils::tftColl, 
     orgdb=org.Hs.eg.db::org.Hs.eg.db, gwrngs=TFutils::gwascat_hg19_chr17) {
reqNS("S4Vectors")
MAPPED_GENE <- CHR_ID <- `DISEASE/TRAIT` <- NULL
CHR_POS <- REGION <- NULL
allst1 = unlist(lapply(gscoll[ grep(tfstub, names(gscoll)) ], GSEABase::geneIds))
st1syms = AnnotationDbi::mapIds(orgdb, keys=allst1, keytype="ENTREZID", column="SYMBOL")
chk = as(S4Vectors::mcols(gwrngs), "data.frame") %>% 
     filter(MAPPED_GENE %in% st1syms) %>% select(`DISEASE/TRAIT`, MAPPED_GENE, CHR_ID, CHR_POS, REGION)
cbind(TF=tfstub, chk)
}

