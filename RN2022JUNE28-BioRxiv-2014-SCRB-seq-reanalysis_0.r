#Rachel Yuan Nong Mikkelsen 2022-JUNE-19
#2022-JUNE-28 copied and modified to plot neighbour (left2 and right3) genes 

print("gene_:")
gene_ <- readline()
a <- table_a
~
~
gene_0 <- ref_human[which(ref_human[,2] == gene_),]
chro_ <- as.character(gene_0[[4]])
chro_ <- ref_human[which(ref_human[,4] == chro_),]
chro_[,6] <- as.numeric(as.character(chro_[,6]))
chro_ <- chro_[order(chro_[,6]),]
gene_0 <- which(chro_[,2] == gene_)
gene_0 <- chro_[(gene_0-20):(gene_0+20), 2]
gene_0 <- data_D1[match(gene_0, rownames(data_D1), nomatch = 0),]
gene_0 <- rownames(gene_0)
gene_ <- which(gene_0 == gene_)
gene_0 <- gene_0[(gene_-2):(gene_+3)]


for(kkki in 1:6){
gene_ <- as.character(gene_0[kkki])
file_name <- paste0(pre_fix, "-", kkki, "--", gene_, "___.pdf")
pdf(file = file_name, height = 4)
par(mfrow = c(2,4), cex = 0.3)
for(i in 1:dim(a)[1]){
        ki <- as.character(a[i,1])
        print_ <- paste0(">>>> ", ki)
        print(print_)
        matrix_out <- data_D1[, which(labels.D1[,3] == ki)]
        matrix_out <- norm.log.mat(matrix_out)
        
        expression_0 <- matrix_out[gene_,]
        if(sum(expression_0) == 0){
                print("expression -")
                expression_ <- c(1:length(expression_0))
		expression_ <- as.data.frame(expression_)
                expression_[,1] <- -1
                expression_ <- expression_[,1]
        }
        if(sum(expression_0) != 0){
                expression_ <- matrix_out[gene_,]
		expression_ <- expression_[order(expression_)]
        }
        par(cex = 0.3)
        plot(expression_[which(expression_ != 0)], col = "red", main = gene_, ylab = "cpm log")
        par(cex = 0.5)
        title(sub = ki)

        access_ <- table(expression_ != 0)
        access_ <- access_[["TRUE"]]
        access_ <- round(access_/length(expression_)*10000)/100
        access_ <- paste0(access_, "% ")
        legend("topleft", legend = access_, text.col = "red", bty = "n")
}
dev.off()
}
