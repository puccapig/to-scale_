#Rachel Yuan Nong Mikkelsen 2021-02-16 Uppsala Sweden
#2021-03-29 copied and modified to work in this file
#reference datasets are drawn from those chosen by Human_Protein_Atlas/Cell_atlas, 1000 cells each...
#add HCA ref_human in RN2021OCT20...-0.r file
#2021-DEC-25 modified to autoplot
#2021-DEC-25 modified to check expression of more_genes in this datasets

print("...to view genes in file:")
more_genes <- readline()
gene_to_views <- read.csv(more_genes)
gene_to_views <- as.data.frame(gene_to_views[,2])

human_cell_tsne_file <- read.csv("RN2021MAR29_scRNAseq-human-cells_refHPAlist_.csv", sep = ";", header = FALSE)
human_cell_tsne_file <- human_cell_tsne_file[2:dim(human_cell_tsne_file)[1],]
human_cell_tsne_file[,2] <- paste0("/Users/rachelnong/Downloads/today_temp/RN2021MAR26-scRNAseq-ref-HPA_", "/", human_cell_tsne_file[,1], "/", human_cell_tsne_file[,2])
human_cell_tsne_file[,3] <- paste0("/Users/rachelnong/Downloads/today_temp/RN2021MAR26-scRNAseq-ref-HPA_", "/", human_cell_tsne_file[,1], "/", human_cell_tsne_file[,3])

ref_human <- read.csv("/Users/rachelnong/Downloads/today_temp/ENSEMBL_release98_gtf_homo_sapiens/Homo_sapiens.GRCh38.98.gtf.csv", header = FALSE)
#gene1_to_views[2] <- as.character(ref_human[which(ref_human[,3] == gene1_to_views),2])
gene_to_views[,2] <- as.character(ref_human[match(as.character(gene_to_views[,1]), ref_human[,3]), 2])

print("to plot number of genes:")
print(dim(gene_to_views)[1])

table_ii <- dim(gene_to_views)[1]
table_kk <- dim(human_cell_tsne_file)[1]
table_ii_kk <- matrix(0, nrow = table_kk, ncol = table_ii)
rownames(table_ii_kk) <- human_cell_tsne_file[,2]

for(ii in 1:dim(gene_to_views)[1]){
	gene1_to_views <- c(as.character(gene_to_views[ii,1]), as.character(gene_to_views[ii,2]))
	
	for(kk in 1:dim(human_cell_tsne_file)[1]){
		gene1_to_views1 <- c("__")
		matrix_in <- human_cell_tsne_file[kk,2]
		matrix_in <- read.csv(matrix_in, header = TRUE)
		rownames(matrix_in) <- matrix_in[,1]
		matrix_in <- matrix_in[,2:dim(matrix_in)[2]]
		expression_code <- rownames(matrix_in)
		
		if(nchar(rownames(matrix_in)[1]) != 15[1]){gene1_to_views1 <- gene1_to_views[1]}
		if(nchar(rownames(matrix_in)[1]) == 15[1]){gene1_to_views1 <- gene1_to_views[2]}
		expression_code <- expression_code[match(gene1_to_views1, expression_code, nomatch = 0)]
		if(length(expression_code == 1)){
			expression_code <- matrix_in[match(gene1_to_views1, rownames(matrix_in), nomatch = 0),]
			if(sum(expression_code) != 0){
				table_ii_kk[kk,ii] <- 1000
			}
			if(sum(expression_code) == 0){
				table_ii_kk[kk,ii] <- 0
			}
		}else{
		table_ii_kk[kk,ii] <- -1
	}
	}
	}
	rownames(table_ii_kk) <- gene_to_views[,1]
	write.csv(table_ii_kk, file = "table_ii_kk.csv")
print("table_ii_kk.csv")

