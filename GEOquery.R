#Week 4 Feb 10 - 15 
#GOAL: Download bioconductor package and use it. 
#Following along with the GEOquery package 

source("https://bioconductor.org/biocLite.R")
#biocLite() way to install bioconductor 
#https://www.bioconductor.org/install/ Info on bioconductor package 


library(BiocInstaller)
library(BiocGenerics)

#Microarray data mining using Bioconductor packages. 

#"""
#The NCBI Gene Expression Omnibus (GEO) serves as a public repository
#for a wide range of high-throughput experimental data.
# These data include single and dual channel microarray-based experiments
# measuring mRNA, genomic DNA, and protein abundance, as well as non-array
# techniques such as serial analysis of gene expression (SAGE)
# mass spectrometry proteomic data, and high-throughput sequencing data.
# At the most basic level of organization of GEO, there are four basic entity 
# types.
# The first three (Sample, Platform, and Series) are supplied by users;
# the fourth, the dataset is compiled and curated by GEO staff from the
# user-submitted data. """ - www.bioconductor.org/packages/devel/bioc/vignettes/GEOquery/inst/doc/GEOquery.html




biocLite("GEOquery")
library(GEOquery)

#Free to access any GEO accession 
gds <- getGEO(filename=system.file("extdata/GDS507.soft.gz",package="GEOquery"))
gsm <- getGEO(filename=system.file("extdata/GSM11805.txt.gz",package="GEOquery"))

#^ you can also do this by getGEO 




head(Meta(gsm))
#first 5 rows of GSM 
Table(gsm)[1:5,]
Columns(gsm)
Columns(gds)[,1:3]


gse <- getGEO(filename=system.file("extdata/GSE781_family.soft.gz",package="GEOquery"))



names(GSMList(gse))
GSMList(gse)[[1]]


names(GPLList(gse))


gse2553 <- getGEO('GSE2553',GSEMatrix=TRUE)
show(gse2553)


