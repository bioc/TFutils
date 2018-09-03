library(TFutils)
library(GSEABase)
library(GenomicRanges)

context("table/range generation")

test_that("grabTab returns expected records", {
  tt = grabTab("VDR", gscoll=TFutils::tftColl,
     orgdb=org.Hs.eg.db::org.Hs.eg.db, 
     gwrngs=TFutils::gwascat_hg19_chr17)
  expect_true(all(dim(tt)==c(28,6)))
})

test_that("tfbs_as_GRanges retrieves scored intervals from local bed files", {
 localtf = system.file("M5946_1/chr1.bed.gz", package="TFutils")
 gr = GenomicRanges::GRanges("chr1", IRanges::IRanges(1e6,11e6))
 oo = importFIMO(Rsamtools::TabixFile(localtf), parms= gr)
 expect_true(length(oo)==10)
})

test_that("genemodelDF generates expected number of records", {
 if (requireNamespace("EnsDb.Hsapiens.v75")) {
  orm = genemodelDF("ORMDL3", EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75)
  expect_true(all(dim(orm)==c(29,9)))
  }
})

context("URL generation")

test_that("URL_s3_tf finds supplied gene symbol", {
 expect_true(URL_s3_tf("VDR") == "http://s3.amazonaws.com/biocfound-tfbs/M5946_1.02sort.bed.gz")
})

test_that("URL_s3_tf finds supplied gene symbol", {
 expect_true(URL_s3_tf("M5946_1") == "http://s3.amazonaws.com/biocfound-tfbs/M5946_1.02sort.bed.gz")
})
