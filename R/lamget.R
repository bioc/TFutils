# Update Oct 31 2020?
cell_url = function() "https://www.cell.com/cms/10.1016/j.cell.2018.01.029/attachment/ede37821-fd6f-41b7-9a0e-9d5410855ae6/mmc2.xlsx"

#' acquire the Excel spreadsheet content for table S1 of Lambert et al. Cell 2018, "The Human Transcription Factors"
#' @import BiocFileCache
#' @import readxl
#' @param cache a BiocFileCache instance
#' @return a tbl_df
#' @note This will download the spreadsheet if not found in `cache`.
#' @examples
#' if (interactive()) retrieve_lambert_main()
#' @export
retrieve_lambert_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
 uuid_element = "ede37821-fd6f"
 peek = BiocFileCache::bfcquery(cache, uuid_element)
 if(length(peek$rpath)==0) 
   BiocFileCache::bfcadd(cache, 
    cell_url())
 peek = try(BiocFileCache::bfcquery(cache, uuid_element))
 if (inherits(peek, "try-error") || length(peek$rpath)==0) stop('could not retrieve xlsx')
 ans = suppressMessages(suppressWarnings(readxl::read_xlsx(peek$rpath, skip=1, sheet=2)))
 names(ans)[4] = "Is TF?"
 ans
}

#' use DT::datatable to browse the Lambert table S1
#' @param cache a BiocFileCache instance
#' @note PMIDs are converted to HTML anchors and DT::datatable is run with `escape=FALSE`.
#' @return result of DT::datatable
#' @examples
#' if (interactive()) browse_lambert_main()
#' @export
browse_lambert_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
 tab = as.data.frame(retrieve_lambert_main(cache=cache))
 tab = anchor_pmids(tab)
 DT::datatable(tab, escape=FALSE)
}
