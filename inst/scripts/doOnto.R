library(ontoProc)
clo = getCellLineOnto()
encode690$lcell = tolower(encode690$cell)
clomat = clo$name[ which(gsub(" cell", "", tolower(clo$name)) %in% 
   intersect(tolower(encode690$cell), gsub(" cell", "", tolower(clo$name))))]
clom = data.frame(tag=names(clomat), name=as.character(clomat))
rownames(clom) = make.names(tolower(gsub(" cell", "", clom$name)), unique=TRUE)
clom[encode690$lcell,] -> dd
encode690$tag = as.character(dd$tag)
encode690$CLOname = as.character(dd$name)
