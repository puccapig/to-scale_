#Rachel Yuan Nong 2020-Oct-08
#to plot many tsne plot in one sheet by gene expression 
#2020-Oct-09 copied and modified to work in this file. human genome
#2020-oct-24 modified to print title
 
print(" mx_tsne_plots_en | (slot_name, slot_number, my_tsne_co) ")
mx_tsne_plots_en <- function(slot_name, slot_number, my_tsne_co){
library(RColorBrewer)
my.pal <- brewer.pal(9, "Set1")
col.from.target <-
function(targets, values) {
      v <- (values-min(values))/(max(values)-min(values))
    targets.rgb <- col2rgb(targets)
    res <- vector("character", length=length(targets))
    for(i in 1:length(values)) {
      mytarget.rgb <- 255-t(unlist(apply(targets.rgb, 1, function(values) {(255-values) * v[i]})))
      res[i] <- rgb(red=mytarget.rgb[1]/256, green=mytarget.rgb[2]/256, blue=mytarget.rgb[3]/256)
    }
    return(res)
  }


more_genes <- read.csv(slot_name, header = TRUE)
my_tsne <- read.csv(my_tsne_co)

ref_human <- read.csv("", header = TRUE)
ref_human <- ref_human[,2:dim(ref_human)[2]]

print("number of genes read from file:")
print(dim(more_genes)[1])

print("use format ensg[1] abbrevation[2]")
format <- readline()
format <- as.numeric(format)
ensg_gene <- ref_human[match(more_genes[,2], ref_human[,format], nomatch = 0),1]
ensg_gene <- as.character(ensg_gene)

ensg_gene <- ensg_gene[match(rownames(matrix_in), ensg_gene, nomatch = 0)] 

Abbrev_gene <- ref_human[match(ensg_gene, ref_human[,1], nomatch = 0), 2]
Abbrev_gene <- as.character(Abbrev_gene)

print("number of those genes read in expression mat")
print(length(Abbrev_gene))

#more_genes <- more_genes[match(Abbrev_gene, more_genes[,2], nomatch = 0),]
more_genes <- more_genes[match(ensg_gene, more_genes[,2], nomatch = 0),]
more_genes <- more_genes[order(more_genes[,1]),]
more_genes <- as.data.frame(more_genes)
more_genes[,3] <- more_genes[,2]
more_genes <- as.matrix(more_genes)

for(i in 1:dim(more_genes)[1]){
	more_genes[i,2] <- as.character(ref_human[ref_human[,1] == more_genes[i,3], 2])
	#more_genes[i,2] <- as.character(ref_human[ref_human[,2] == more_genes[i,3], 1])
}
more_genes <- as.data.frame(more_genes)

for(i in 1:dim(more_genes)[1]){
 if(max(matrix_in[as.character(more_genes[i,3]),]) > 0){
 #if(max(matrix_in[as.character(more_genes[i,2]),]) > 0){
        more_genes[i,4] <- 1}
 else{
        more_genes[i,4] <- 0}
}

more_genes <- more_genes[more_genes[,4] == 1, 1:3]
print("number of genes that shown counts")
print(dim(more_genes)[1])

ref_human <- 0

print("n_col:")
n_col <- readline()
n_col <- as.numeric(n_col)
n_row <- round(dim(more_genes)[1]/n_col) + 1
dev.new(); par(mfrow = c(n_row, n_col), cex = 0.2)

for(i in 1:dim(more_genes)[1]){
        look_at_gene <- as.character(more_genes[i,3])
	#look_at_gene <- as.character(more_genes[i,2])
        target <- my.pal[1:length(look_at_gene)]
        values <- matrix_in[match(look_at_gene, rownames(matrix_in), nomatch = 0),]
        cols.by.raw.counts <- col.from.target(target, values)
        
	rplot_label <- paste0(look_at_gene, " | ", more_genes[i,2])
	#rplot_label <- paste0(look_at_gene, " | ", more_genes[i,3])
        plot(my_tsne[,3], my_tsne[,2], pch = 19, col = cols.by.raw.counts, main = rplot_label, xlab = "TSNE1", ylab = "TSNE2", cex = 0.5)
	}

filename <- paste0(pre_fix, "-part", slot_number, "-", look_at_gene, "___.pdf")
pdf(file = filename)
par(mfrow = c(n_row, n_col), cex = 0.2)
for(i in 1:dim(more_genes)[1]){
	look_at_gene <- as.character(more_genes[i,3])
	#look_at_gene <- as.character(more_genes[i,2])
	target <- my.pal[1:length(look_at_gene)]
        values <- matrix_in[match(look_at_gene, rownames(matrix_in), nomatch = 0),]
        cols.by.raw.counts <- col.from.target(target, values)
	
	rplot_label <- paste0(look_at_gene, " | ", more_genes[i,2])
	#rplot_label <- paste0(look_at_gene, " | ", more_genes[i,3])
	plot(my_tsne[,3], my_tsne[,2], pch = 19, col = cols.by.raw.counts, main = rplot_label, xlab = "TSNE1", ylab = "TSNE2", cex = 0.5)
	}
	dev.off()

}
