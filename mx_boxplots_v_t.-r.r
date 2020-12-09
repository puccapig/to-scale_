#Rachel Yuan Nong 2020-JANUARY-07
#This script is written to boxplot mouse gene expression  
#scRNAseq data includes datasets (downsize)part of Tabula Muris, Odom-2019-NatureCommunications-BioRxiv-2018, and 10xgenomics mouse PBMC.
#2020-januari-09: modified to save plot in pdf file.
#2020-januari-22: modified to work in this file
#2020-sept-21: modified to work in this file
#2020-okt-24: modified to print title

print(" mx_boxplots_v_t | (labels.experiments, slot_number, slot_name) ")
mx_boxplots_v_t <- function(labels.experiments, slot_number, slot_name){
more_genes <- read.csv(slot_name, header = TRUE)

ref_human <- read.csv("", header = FALSE) 

print("number of genes read from file:")
print(dim(more_genes)[1])

#print("USE: ensembl[1], abbrev[2]")
#format <- readline()
#format <- as.numeric(format)
format <- 2

ensg_gene <- ref_human[match(more_genes[,2], ref_human[,format], nomatch = 0), 1]
ensg_gene <- as.character(ensg_gene)
ensg_gene <- ensg_gene[match(rownames(matrix_in), ensg_gene, nomatch = 0)]

Abbrev_gene <- ref_human[match(ensg_gene, ref_human[,1], nomatch = 0), 2]
Abbrev_gene <- as.character(Abbrev_gene)

print("number of those genes read in expression mat")
print(length(ensg_gene))

more_genes <- more_genes[match(Abbrev_gene, more_genes[,2], nomatch = 0),]
more_genes <- more_genes[order(more_genes[,1]),]
more_genes[,3] <- more_genes[,2]
more_genes <- as.matrix(more_genes)
for(i in 1:length(ensg_gene)){
        more_genes[i,2] <- ensg_gene[Abbrev_gene == more_genes[i,3]]
}
more_genes <- as.data.frame(more_genes)

for(i in 1:dim(more_genes)[1]){
 if(max(matrix_in[as.character(more_genes[i,2]),]) > 0){
        more_genes[i,4] <- 1}
 else{
        more_genes[i,4] <- 0}
}

more_genes <- more_genes[more_genes[,4] == 1, 1:3]
print("number of genes that shown counts")
print(dim(more_genes)[1])

ref_human <- 0

dev.new(); par(mfrow = c(5, 2), cex = 0.3)

ave.gene <- matrix_in[as.character(more_genes[,2]),]
rownames(ave.gene) <- more_genes[,3]

ave.gene <- t(ave.gene)
ave.gene <- as.matrix(ave.gene)

for(i in 1:length(table(labels.experiments))){
        plot_part <- names(table(labels.experiments))[i]
        plot_name <- paste0(plot_part, ": ", dim(more_genes)[1], "genes")
        boxplot(ave.gene[labels.experiments == plot_part,], col = rainbow(dim(ave.gene)[2]), xlab = plot_name, ylab = c("log expression cpm"))
}       

filename <- paste0(pre_fix, "-", "part_boxplot", slot_number, "_", "___.pdf")
pdf(file = filename)
par(mfrow = c(5, 2), cex = 0.1)
for(i in 1:length(table(labels.experiments))){
        plot_part <- names(table(labels.experiments))[i]
        plot_name <- paste0(plot_part, ": ", dim(more_genes)[1], "genes")
        boxplot(ave.gene[labels.experiments == plot_part,], col = rainbow(dim(ave.gene)[2]), xlab = plot_name, ylab = c("log expression cpm"))
}
        dev.off()
}
