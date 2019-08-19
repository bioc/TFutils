# this code attempts to transform the PMIDs in 
# comments into suitable HTML anchors

eight_digits_regexp = function(x) 
    "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]"
seven_digits_regexp = function(x) 
    "[0-9][0-9][0-9][0-9][0-9][0-9][0-9]"
re78 = function() paste0(seven_digits_regexp(), "|", eight_digits_regexp()) 
  
pmid_inds = function(x) grep(re78(), x)
  
isolate_pmid = function(x) {
   stopifnot(is.atomic(x), length(x)==1)
   cand = strsplit(x, ": |\\)|:|\\]|;|'|\\(|\\[")[[1]]
   cand[grep(re78(), cand)]
  }
  
fabricate_anchor = function(x)
   sprintf("<a href=http://pubmed.gov/%s>%s</a>", x, x)
  
ancize_column = function(x) {
  comi = pmid_inds(x)
  for (i in comi) {
    jj = x[i]
    kk = sapply(jj, isolate_pmid)
    repl = sapply(kk, fabricate_anchor)
    for (j in seq_along(kk))
      x[i] = gsub(kk[j], repl[j], x[i])
    }
    x
  }
  
#' check columns of a dataframe for numerical tokens of 7 or 8 digits
#' and create HTML anchors to pubmed.gov constituting a link to a PMID
#' @param dataframe a data.frame instance
#' @return data.frame with HTML anchors to pubmed.gov inserted where 7- or 8-digit numbers are found
#' @note The method of isolating putative PMIDs is peculiar to patterns found in
#' the comment fields of annotated TF table (supplemental table S1 found in
#' \url{https://www.cell.com/cms/10.1016/j.cell.2018.01.029/attachment/88c0eca1-66f9-4068-b02e-bd3d55144f79/mmc2.xlsx} of PMID 29425488).  When DT::datatable is called on the output
#' of this function with `escape=FALSE` the PMIDs will render as hyperlinks.
#' Note that column 1 is assumed to be an ENSEMBL ID which could have 7 or 8 digits but is handled differently
#' @examples
#' litdf = data.frame(id="ENSG00000116819", a="Binds the same GCCTGAGGC sequence as the other AP-2s (PMID: 24789576)",
#'      stringsAsFactors=FALSE)
#' anchor_pmids(litdf)
#' @export
anchor_pmids = function(dataframe) {
  haspmid = sapply(dataframe, function(x) length(grep(re78(), x))>0)
  haspmid[1] = FALSE  # don't do ensembl IDs
  for (i in 1:ncol(dataframe)) {
   if (haspmid[i]) dataframe[,i] = ancize_column(dataframe[,i])
  }
  dataframe[,1] = sprintf("<A HREF='https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=%s'>%s</A>",
        dataframe[,1], dataframe[,1])
  dataframe
}
