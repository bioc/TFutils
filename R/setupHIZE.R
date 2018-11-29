#' process a gene_attribute_matrix.txt file from harmonizeome into a GeneSetCollection
#' @param txtfn character(1) path to gene_attribute_matrix.txt file from harmonizeome
#' @param tag character(1) will be added to shortDescription field of each GeneSet instance
#' @note After uncompressing content of \url{http://amp.pharm.mssm.edu/static/hdfs/harmonizome/data/cheappi/gene_attribute_matrix.txt.gz} run this on gene_attribute_matrix.txt with tag="CHEA".
setupHIZE = function(txtfn="gene_attribute_matrix.txt", tag) {
# http://amp.pharm.mssm.edu/static/hdfs/harmonizome/data/cheappi/gene_attribute_matrix.txt.gz
 dd = read.delim(txtfn, sep="\t", skip=3, h=FALSE, stringsAsFactors=FALSE)
 dat = data.matrix(dd[,-c(1:3)])
 x = readLines(txtfn, n=3)
 tfn = strsplit(x[1], "\\t")[[1]]
 tfna = tfn[-c(1:3)]
 rownames(dat) = dd[,1]
 colnames(dat) = tfna
 targs = lapply(tfna, function(x) rownames(dat)[which(dat[,x]==1)]) 
 names(targs) = tfna
 cheaTargs = targs
 ch = lapply(cheaTargs, function(x) {
    tmp = GSEABase::GeneSet(GSEABase::SymbolIdentifier(), geneIds=x)
    tmp@shortDescription = paste(
      tmp@shortDescription, "(from", tag, ")")
    tmp
    })
 ch2 = lapply(names(ch), function(x){ GSEABase::setName(ch[[x]])=x; ch[[x]]})
 GSEABase::GeneSetCollection(ch2)
}
