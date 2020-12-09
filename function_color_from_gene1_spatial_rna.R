#Rachel Yuan Nong 20190628 from function.R.
#modified 2019-AUG-26, 2019-SEPT-01, 2019-OKT-16, 2019-DEC-05
#this is modified for looking at sum of raw counts per cell
#2019-DEC-05: adapted to plot visium tissue data
#2020-NOV-02: copied and modified to work with find_region_specific_genes

print("Quit and assign new pre_fix otherwise.")
print(pre_fix)
print("function in use: col.from.target")
print("[ENSMUSG] look_at_gene:"); look_at_gene <- readline()
print("[gene abbreviation] name2:"); name2_gene <- readline()
#print("Describe plot as: RN[YY][MM][DD]-[ref]-[tissue sample]_")
#pre_fix <- readline


library(RColorBrewer)
my.pal <- brewer.pal(9, "Set1")

target <- my.pal[1:length(look_at_gene)]
values <- matrix_in[match(look_at_gene, rownames(matrix_in), nomatch = 0),]

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

#dev.new(); par(cex = 0.5)
#print("Describe plot as: RN[YY][MM][DD]-[ref]-[tissue sample]_")
#pre_fix <- readline()
rplotname <- paste0(name2_gene, " | ",  look_at_gene)

par(cex = 0.5)
plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.gene.st[,1], xlab = rplotname, cex = 0.7)

filename <- paste0(pre_fix, "-", look_at_gene, "____.pdf")
pdf(file = filename)
plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.gene.st[,1], xlab = rplotname, cex = 0.7)
dev.off()

