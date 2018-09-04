#' define a structure to hold information about TFs from diverse reference sources
#' @importFrom methods new show
#' @importFrom stats na.omit
#' @slot name character 
#' @slot nativeIds character tokens used by the provider to enumerate transcription factors
#' @slot HGNCmap data.frame with atleast two columns,
#' native id as first column and HGNC symbol as second column
#' @slot metadata ANY
#' @note This class respects the notions that 1) a source of information
#' about transcription factors should have a name, 2) each source
#' has its own 'native' nomenclature for the factors themselves,
#' 3) it is common to use the gene symbol to refer to the transctiption
#' factor, and 4) additional metadata will frequently be required to
#' establish information about provenance of assertions about transcription
#' factors.
#' @aliases "TFCatalog-class"
#' @export
setClass("TFCatalog", representation(name="character",
                                     nativeIds="character", HGNCmap="data.frame", metadata="ANY"))
#' Constructor for TFCatalog
#' @param name informative character(1) for collection
#' @param nativeIds character() vector of identifiers used by collection creators
#' @param HGNCmap data.frame with column 1 nativeIds, column 2 HGNC or hgnc.heur for MSigDb
#' and any other columns of use
#' @param metadata a list of metadata elements
#' @return instance of TFCatalog
#' @examples
#' if (require("GSEABase")) {
#'  TFs_MSIG = TFCatalog(name="MsigDb.TFT",nativeIds=names(TFutils::tftColl),
#'  HGNCmap=data.frame(TFutils::tftCollMap,stringAsFactors=FALSE))
#'  TFs_MSIG
#' }
#' @export
TFCatalog = function(name, nativeIds, HGNCmap, metadata) {
  if (missing(metadata)) metadata=list()
  new("TFCatalog", name=name, nativeIds=nativeIds,
      HGNCmap=HGNCmap, metadata=metadata)
}
#' simple accessor for HGNCmap component of TFCatalog
#' @importFrom methods slot
#' @param x instance of TFCatalog
#' @return dataframe instance
#' @examples
#' HGNCmap
#' @export
HGNCmap = function(x) slot(x, "HGNCmap")
#' produce a concise report on TFCatalog instance
#' @aliases show,TFCatalog-method
#' @return side effect
#' @param object instance of TFCatalog
#' @export
setMethod("show", "TFCatalog", function(object) {
  reqNS("Biobase")
  cat("TFutils TFCatalog instance", object@name, "\n")
  cat(sprintf(" %d native Ids, including\n   ", length(object@nativeIds)))
  cat(Biobase::selectSome(object@nativeIds, max=2), "\n")
  cat(sprintf(" %d unique HGNC tags, including\n   ", length(unique(object@HGNCmap[,2]))))
  cat(Biobase::selectSome(na.omit(object@HGNCmap[,2])), "\n")
})
