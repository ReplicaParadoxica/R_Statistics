if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("biomaRt"))
library(biomaRt)

install.packages("seqinr")
library(seqinr)
choosebank("genbank")
query("BRCA1", "SP=Homo sapiens AND K=BRCA1")
