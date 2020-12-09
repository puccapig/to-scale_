#Rachel Yuan Nong 20190628 from function.R.
#modified 2019-AUG-26, 2019-SEPT-01, 2019-OKT-16, 2019-DEC-05
#this is modified for looking at sum of raw counts per cell
#2019-DEC-05: adapted to plot visium tissue data
#2019-DEC-06: adapted to plot visium tissue overview.
#2019-DEC-16: modified to work with visium human tissue data.
#2020-OCT-12: copied and modified to work in this file.

tissue_position <- read.csv("spatial/tissue_positions_list.csv")
source("10xgenomics_visium_st_loading_matrices dot R")

print("R: imported mat:")
dim(mat)

source("scale.mat.cpm.R")

print("R: mat to cpm_mat...")
cpm_mat <- norm.log.mat(mat)

print("R: to pc_cpm_mat | returning pc_cpm_mat_a...")
pc_cpm_mat <- prcomp(cpm_mat)
pc_cpm_mat_a <- pc_cpm_mat[[2]]

library(tsne)
print("R: to my_tsne_pc_cpm_mat, input k_tsne")
k_tsne <- readline()
k_tsne <- as.numeric(k_tsne)
my_tsne_pc_cpm_mat <- tsne(pc_cpm_mat_a[,1:10], k = k_tsne)

print("R: to clusters, input k_cluster...")
k_cluster <- readline()
k_cluster <- as.numeric(k_cluster)
cutree_tsne_cpm_mat_10x_human <- cutree(hclust(dist(my_tsne_pc_cpm_mat)), k = k_cluster)

cols.cutree_tsne_10x_human <- rainbow(length(table(as.factor(cutree_tsne_cpm_mat_10x_human))))[as.integer(as.factor(cutree_tsne_cpm_mat_10x_human))]
print("R: done coloring clusters as cols.cutree_tsne_10x_human mat order.")

cols.by.tsne.st <- c(1:4992)
cols.by.tsne.st <- as.data.frame(cols.by.tsne.st)
colnames(cols.by.tsne.st) <- c("cols.by.tsne.st")

for(i in 1:4992){
a <- as.character(tissue_position[i,1])
if(match(a, colnames(cpm_mat), nomatch = 0) == 0){cols.by.tsne.st[i,1] <- 0}
if(match(a, colnames(cpm_mat), nomatch = 0) != 0){cols.by.tsne.st[i,1] <- cols.cutree_tsne_10x_human[match(a, colnames(cpm_mat), nomatch = 0)]}
}

plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.tsne.st[,1], cex = 0.7)

print("have a look at the plot...")
