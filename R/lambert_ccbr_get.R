# Update Oct 31 2020?
ccbr_cell_url = function() "http://humantfs.ccbr.utoronto.ca/download/v_1.01/DatabaseExtract_v_1.01.csv"

#' acquire the CSV content for table S1 of Lambert et al. Cell 2018, "The Human Transcription Factors" from the Human TFS website
#' @import BiocFileCache
#' @import readxl
#' @param cache a BiocFileCache instance
#' @return a tbl_df
#' @note This will download the spreadsheet if not found in `cache`.
#' @examples
#' if (interactive()) retrieve_lambert_main()
#' @export
#' 
#' 
retrieve_humantfs_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
 uuid_element = "humantfs.ccbr.utoronto.ca/download/v_1.01/DatabaseExtract_v_1.01.csv"
 peek = BiocFileCache::bfcquery(cache, uuid_element)
 if(length(peek$rpath)==0) 
   BiocFileCache::bfcadd(cache, 
    ccbr_cell_url())
 peek = try(BiocFileCache::bfcquery(cache, uuid_element))
 if (inherits(peek, "try-error") || length(peek$rpath)==0) stop('could not retrieve csv')
 ans = suppressMessages(suppressWarnings(read.csv2(peek$rpath, header = TRUE, stringsAsFactors = FALSE, sep = ","))) 
 ans
}

    

#' use DT::datatable to browse the Lambert table S1
#' @param cache a BiocFileCache instance
#' @note PMIDs are converted to HTML anchors and DT::datatable is run with `escape=FALSE`.
#' @return result of DT::datatable
#' @examples
#' if (interactive()) browse_lambert_main()
#' @export
browse_humantfs_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
 tab = as.data.frame(retrieve_humantfs_main(cache=cache))
 tab = tab[ , -1] 
 idx = tab$Is.TF. == 'Yes'
 tab = tab[ idx, ]
# tab = anchor_pmids(tab)
 DT::datatable(tab, escape=FALSE)
}

