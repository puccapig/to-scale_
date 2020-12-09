#Rachel Yuan Nong 20190628 from function.R.
#modified 2019-AUG-26, 2019-SEPT-01, 2019-OKT-16, 2019-DEC-05
#this is modified for looking at sum of raw counts per cell
#2019-DEC-05: adapted to plot visium tissue data
#2019-DEC-18: rewrite to plot visium tissue data on total umi counts and number of detected genes on each spot 
#2020-NOV-02: note in previous versions /cols.by.row.counts/ refers to /cols.by.raw.counts/. 

print("Quit and assign new pre_fix otherwise.")
print(pre_fix)
print("function in use: col.from.target")
#print("Quit and assign cpm_mat to matrix_in otherwise.")

total_umi <- colSums(mat)
total_umi <- as.data.frame(t(total_umi))
rownames(total_umi) <- c("total_umi")

library(RColorBrewer)
my.pal <- brewer.pal(9, "Set1")

target <- my.pal[1:1]
values <- total_umi[1,]

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

cols.by.raw.counts <- col.from.target(target, values)

print("cols.by.raw.counts")
print(length(cols.by.raw.counts))

cols.by.gene.st <- c(1:4992)
cols.by.gene.st <- as.data.frame(cols.by.gene.st)
colnames(cols.by.gene.st) <- c("cols.by.gene.st")

for(i in 1:4992){
a <- as.character(tissue_position[i,1])
if(match(a, colnames(cpm_mat), nomatch = 0) == 0){cols.by.gene.st[i,1] <- 0}
if(match(a, colnames(cpm_mat), nomatch = 0) != 0){cols.by.gene.st[i,1] <- cols.by.raw.counts[match(a, colnames(cpm_mat), nomatch = 0)]}
}

rplotname <- paste0(rownames(total_umi), "_on_each_spot")
filename <- paste0(pre_fix, "-", rownames(total_umi), "____.pdf")
pdf(file = filename)
plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.gene.st[,1], xlab = rplotname, cex = 0.7)
dev.off()

source("count_gene_expressed.R")

detected_genes <- colSums(b)
print("return genes detection 1/0 in b")

detected_genes <- as.data.frame(t(detected_genes))
rownames(detected_genes) <- c("detected_genes")
values <- detected_genes[1,]
cols.by.raw.counts <- col.from.target(target, values)
print("cols.by.raw.counts")
print(length(cols.by.raw.counts))
cols.by.genes.st <- c(1:4992)
cols.by.genes.st <- as.data.frame(cols.by.genes.st)
colnames(cols.by.genes.st) <- c("cols.by.genes.st")

for(i in 1:4992){
a <- as.character(tissue_position[i,1])
if(match(a, colnames(cpm_mat), nomatch = 0) == 0){cols.by.genes.st[i,1] <- 0}
if(match(a, colnames(cpm_mat), nomatch = 0) != 0){cols.by.genes.st[i,1] <- cols.by.raw.counts[match(a, colnames(cpm_mat), nomatch = 0)]}
}

rplotname <- paste0(rownames(detected_genes), "_on_each_spot")
filename <- paste0(pre_fix, "-", rownames(detected_genes), "____.pdf")
pdf(file = filename)
plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.genes.st[,1], xlab = rplotname, cex = 0.7)
dev.off()


