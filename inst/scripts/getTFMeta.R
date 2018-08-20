# "https://genome.ucsc.edu/cgi-bin/hgTables?hgsid=686726561_9aeujyhQXuUhZyNoDYHyRzxA6TPa&clade=mammal&org=Human&db=hg19&hgta_group=allTables&hgta_track=hg19&hgta_table=metaDb&hgta_regionType=genome&position=chr21%3A33%2C031%2C597-33%2C041%2C570&hgta_outputType=primaryTable&hgta_outFileName=hgtab.txt"
library(data.table)
x = fread("hgtab.txt")
xdf = as.data.frame(x)
library(dplyr)
library(magrittr)
names(xdf)[1] = "obj"
library(tibble)
as.tibble(xdf) %>% filter(var %in% c("cell")) -> celtab
as.tibble(xdf) %>% filter(var %in% c("target")) -> tartab
library(AnnotationHub)
ah = AnnotationHub()
mah = mcols(ah)
mah = cbind(mah, AHID = rownames(mah))
tmah = as.tibble(mah)
trunctitle = gsub("\\..*", "", tmah[,1][[1]])
tmah$obj = trunctitle
inner_join(celtab, tartab, by="obj") -> ct
names(ct) = c("obj", "class1", "cell", "class2", "target")
inner_join(tmah, ct, by="obj") -> mk690
library(S4Vectors)
encode690 = DataFrame(mk690)
metadata(encode690) = list(source="https://genome.ucsc.edu/cgi-bin/hgTables?hgsid=686726561_9aeujyhQXuUhZyNoDYHyRzxA6TPa&clade=mammal&org=Human&db=hg19&hgta_group=allTables&hgta_track=hg19&hgta_table=metaDb&hgta_regionType=genome&position=chr21%3A33%2C031%2C597-33%2C041%2C570&hgta_outputType=primaryTable&hgta_outFileName=hgtab.txt", date=date())
save(encode690, file="encode690.rda")
