#' Update Oct 18 2021
ccbr_cell_url = function() "http://humantfs.ccbr.utoronto.ca/download/v_1.01/DatabaseExtract_v_1.01.csv"

#' acquire the CSV content for table S1 of Lambert et al. Cell 2018 from the Human TFS repository at http://humantfs.ccbr.utoronto.ca 
#' @import BiocFileCache
#' @param cache a BiocFileCache instance
#' @return a tbl_df
#' @note This will download the spreadsheet if not found in `cache`.
#' @examples
#' if (interactive()) retrieve_humantfs_main()
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

    

#' use DT::datatable to browse the Lambert's Human Transcription Factors repository
#' @param cache a BiocFileCache instance
#' @return result of DT::datatable
#' @examples
#' if (interactive()) browse_humantfs_main()
#' @export
browse_humantfs_main = function(cache=BiocFileCache::BiocFileCache(ask=FALSE)) {
 tab = as.data.frame(retrieve_humantfs_main(cache=cache))
 tab = tab[ , -1] 
 idx = tab$Is.TF. == 'Yes' # keep only if TF = Yes 
 tab = tab[ idx, ]
 DT::datatable(tab, escape=FALSE)
}

