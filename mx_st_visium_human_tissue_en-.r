#Rachel Yuan Nong 2020-OCT-12
#this is to be used to plot multiple visium_st views on one sheet
#2020-OCT-12: copied and modified to work in this file.
#2020-okt-24: modified to print title
#2020-nov-02: copied and modified to work with ensembl input format

print(" mx_st_visium_en | (slot_number, slot_name) ")
print("total slots:")
total_slot <- readline()

library(RColorBrewer)
my.pal <- brewer.pal(9, "Set1")

col.from.target <- function(targets, values) {
      v <- (values-min(values))/(max(values)-min(values))
    targets.rgb <- col2rgb(targets)
    res <- vector("character", length=length(targets))
    for(i in 1:length(values)) {
      mytarget.rgb <- 255-t(unlist(apply(targets.rgb, 1, function(values) {(255-values) * v[i]})))
      res[i] <- rgb(red=mytarget.rgb[1]/256, green=mytarget.rgb[2]/256, blue=mytarget.rgb[3]/256)
    }
    return(res)
  }


mx_st_visium_en <- function(slot_number, slot_name){
more_genes <- read.csv(slot_name, header = TRUE)

ref_human <- read.csv("")
ref_human <- ref_human[,2:dim(ref_human)[2]]

print("USE: ensembl[1], abbrev[2]")
#format <- readline()
#format <- as.numeric(format)
format <- 1

ensg_gene <- ref_human[match(more_genes[,2], ref_human[,format], nomatch = 0), 1]
ensg_gene <- as.character(ensg_gene)

ensg_gene <- rownames(matrix_in[match(ensg_gene, rownames(matrix_in), nomatch = 0),])
print("number of genes to look at: ")
print(length(ensg_gene))

ensg_gene <- as.data.frame(ensg_gene)
for(i in 1:dim(ensg_gene)[1]){
        if(max(matrix_in[match(ensg_gene[i,1], rownames(matrix_in), nomatch = 0),]) > 0){
                ensg_gene[i,2] <- 1
                } else {
                ensg_gene[i,2] <- 0
                }
}

ensg_gene <- ensg_gene[ensg_gene[,2] == 1, 1]
abbrev_gene <- ref_human[match(ensg_gene, ref_human[,1], nomatch = 0), 2]
abbrev_gene <- as.character(abbrev_gene)

print("number of genes shown expression in expression matrix:")
print(length(abbrev_gene))

	#assign ncol:h
        h <- 6
        #assign nrow:v
        v <- round(length(abbrev_gene)/h) + 1

	dev.new(); par(mfrow = c(v, h), cex = 0.3)

        for(i in 1:length(ensg_gene)){
                look_at_gene <- ensg_gene[i]
                plot_name <- paste0(ensg_gene[i], " | ", abbrev_gene[i])

                target <- my.pal[1:length(look_at_gene)]
                values <- matrix_in[match(look_at_gene, rownames(matrix_in), nomatch = 0),]
                cols.by.raw.counts <- col.from.target(target, values)

                cols.by.gene.st <- c(1:4992)
                cols.by.gene.st <- as.data.frame(cols.by.gene.st)
                colnames(cols.by.gene.st) <- c("cols.by.gene.st")

        for(i in 1:4992){
                a <- as.character(tissue_position[i,1])
                if(match(a, colnames(cpm_mat), nomatch = 0) == 0){
                        cols.by.gene.st[i,1] <- 0}
                if(match(a, colnames(cpm_mat), nomatch = 0) != 0){
                        cols.by.gene.st[i,1] <- cols.by.raw.counts[match(a, colnames(cpm_mat), nomatch = 0)]}

        }

        plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.gene.st[,1], cex = 0.5)
	title(sub = plot_name, cex.sub = 0.6)
        }
        
	filename <- paste0(pre_fix, "_", ensg_gene[i], "mx", slot_number, "___.pdf")
	pdf(file = filename)
	par(mfrow = c(v, h), cex = 0.3)     
	
	for(i in 1:length(ensg_gene)){
        	look_at_gene <- ensg_gene[i]
        	plot_name <- paste0(ensg_gene[i], " | ", abbrev_gene[i])
        
		target <- my.pal[1:length(look_at_gene)]
		values <- matrix_in[match(look_at_gene, rownames(matrix_in), nomatch = 0),]	
		cols.by.raw.counts <- col.from.target(target, values)
	
		cols.by.gene.st <- c(1:4992)
        	cols.by.gene.st <- as.data.frame(cols.by.gene.st)
        	colnames(cols.by.gene.st) <- c("cols.by.gene.st")
	
	for(i in 1:4992){
                a <- as.character(tissue_position[i,1])
                if(match(a, colnames(cpm_mat), nomatch = 0) == 0){
                        cols.by.gene.st[i,1] <- 0}
                if(match(a, colnames(cpm_mat), nomatch = 0) != 0){
                        cols.by.gene.st[i,1] <- cols.by.raw.counts[match(a, colnames(cpm_mat), nomatch = 0)]}

	}

        plot(tissue_position[,6], -tissue_position[,5], pch = 19, col = cols.by.gene.st[,1], cex = 0.5)
        title(sub = plot_name, cex.sub = 0.6)
	}
        dev.off()
}

for(i in 1:total_slot){
        slot_number <- i
        slot_name <- paste0("new_slot_", i, ".csv")
        mx_st_visium(slot_number, slot_name)
}

