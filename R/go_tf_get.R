# Last updated 2021 Oct 12

gotf_url = function() "https://www.biorxiv.org/content/biorxiv/early/2020/12/07/2020.10.28.359232/DC1/embed/media-1.xlsx"

#' acquire the CSV content for table S1 of Lambert et al. Cell 2018, "The Human Transcription Factors" from the Human TFS website
#' @rawNamespace import(AnnotationDbi, except=select)
#' @import BiocFileCache
#' @import readxl
#' @import org.Hs.eg.db
#' @param cache a BiocFileCache instance
#' @return a tbl_df
#' @note This will download the spreadsheet if not found in `cache`.
#' @examples
#' if (interactive()) retrieve_gotf_main()
#' @export
#' 
#' 
retrieve_gotf_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
  uuid_element = "www.biorxiv.org/content/biorxiv/early/2020/12/07/2020.10.28.359232/DC1/embed/media-1.xlsx"
  peek = BiocFileCache::bfcquery(cache, uuid_element)
  if(length(peek$rpath)==0) 
    BiocFileCache::bfcadd(cache, gotf_url())
  peek = try(BiocFileCache::bfcquery(cache, uuid_element))
  if (inherits(peek, "try-error") || length(peek$rpath)==0) stop('could not retrieve gotf')
    ans = suppressMessages(suppressWarnings(readxl::read_xlsx(peek$rpath, skip=0, sheet=2))) 
  ans
}


#' use DT::datatable to browse the Gotf table xxx
#' @param cache a BiocFileCache instance
#' @note PMIDs are converted to HTML anchors and DT::datatable is run with `escape=FALSE`.
#' @return result of DT::datatable
#' @examples
#' if (interactive()) browse_gotf_main()
#' @export
browse_gotf_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
  tab = as.data.frame(retrieve_gotf_main(cache=cache))
  names(tab) <- gsub(" ", "_", names(tab))
  ids <- AnnotationDbi::mapIds(org.Hs.eg.db, tab$HGNC_approved_gene_symbol, "ENSEMBL", "SYMBOL", multiVals="list")
  tab$ENSEMBLID <- ids
  # tab = anchor_pmids(tab)
  DT::datatable(tab, escape=FALSE)
  
}
