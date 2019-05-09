#' set up list of pfms in motifStack protocol
#' @param tffam character(1) name of TF family as found in TFutils::hocomoco.mono field `TF family`
#' @param trimfac fraction passed as parameter `t` to motifStack::trimMotif
#' @note Uses MotifDb, motifStack to create a list of pfms
#' @return a list of pfm instances as defined in motifStack
#' @examples
#' n1 = tffamCirc.prep()
#' str(n1)
#' @export
tffamCirc.prep = function(tffam = "Paired-related HD factors{3.1.3}",
    trimfac = 0.4) {
  chkns = function(p) {
   if (!requireNamespace(p)) 
      stop(sprintf("please install package %s to use this function.", p))
   }
  chk = sapply(c("motifStack", "MotifDb", "dplyr", "magrittr", 
              "RColorBrewer"), chkns)
  "%>%" = magrittr::"%>%"
  prhdvec = dplyr::mutate(TFutils::hocomoco.mono, TFfamily=`TF family`) %>% 
   dplyr::filter(TFfamily == "Paired-related HD factors{3.1.3}") %>% 
   dplyr::select(`Transcription factor`) %>% unlist %>% unname
  #
  #
  cands = lapply(prhdvec, function(x) MotifDb::query(MotifDb::MotifDb, c(x, "jaspar2018", "Hsapiens")))
  ok = sapply(cands, function(x) length(x)>0)
  cands = cands[ok]
  mots = lapply(cands, function(x) as.list(x)[[1]]) # just pick first when multiple
  # pfm class is accessible with requireNamespace
  mots = lapply(1:length(mots), function(x) new("pfm", mots[[x]], name=prhdvec[ok][x]))
  motifs2 = mots
  if (trimfac>0) motifs2 = lapply(mots, motifStack::trimMotif, t=trimfac)
  names(motifs2) = prhdvec[ok]
  motifs2
}

#' use a radial plot (by default) for motif stack
#' @param motiflist a list of pfm instances from motifStack
#' @param circosParms a list of parameter settings for circos plot
#' @return side effect to graphics device
#' @examples
#' p1 = tffamCirc.prep( )
#' tffamCirc.plot(p1[c(1:8, 10:17, 19)])
#' @export
tffamCirc.plot = function(motiflist, circosParms = defaultCircosParms()) {
  ### plot logo stack with radial style
  ans = list(pfms = motiflist)
  ans = c(ans, circosParms)
  do.call(motifStack::motifStack, ans)
}
     
#' basic layout parameters for circos
#' @return a list
#' @examples
#' head(defaultCircosParms())
#' @export
defaultCircosParms = function() {
        color <- RColorBrewer::brewer.pal(12, "Set3")
        list(layout="radialPhylog", 
             circle=0.3, cleaves = 0.2, 
             clabel.leaves = 0.5, 
             col.bg=rep(color, each=5), col.bg.alpha=0.3, 
             col.leaves=rep(color, each=5),
             col.inner.label.circle=rep(color, each=5), 
             inner.label.circle.width=0.05,
             col.outer.label.circle=rep(color, each=5), 
             outer.label.circle.width=0.02, 
             circle.motif=1.2,
             angle=350)
        }

#  motifStack::motifStack(motifs2[c(1:8,10:17,19)], layout="radialPhylog", 
