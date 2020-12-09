#Rachel Yuan Nong 2019-July-18
#count expressed/detected genes using 10x's bc_matrix
#2020-APRIL-30: modified to add descripitons.

#print("rename matrix as matrix_in. ")
#rename_done <- readline()
print(dim(matrix_in))

#input from last line
print("number_of_rows: "); number_of_rows <- readline(); number_of_rows <- as.numeric(number_of_rows)
print("number_of_colums: "); number_of_columns  <- readline(); number_of_columns <- as.numeric(number_of_columns)


i = 0
j = 0
b <- matrix(0, nrow = number_of_rows, ncol = number_of_columns)


for(i in 1:number_of_columns){
      for(j in 1:number_of_rows){
      	    if(matrix_in[j, i] != 0) b[j, i] <- 1 else b[j, i] <- 0
	    }
	   }

print("name matrix_in:")
mat_name <- readline()
mat_name <- as.character(mat_name)

dev.new()
hist(colSums(b), breaks = 20, main = c("expressed genes"))
title(sub = mat_name, cex.sub = 1)

colnames(b) <- colnames(matrix_in)
rownames(b) <- rownames(matrix_in)

print("expressed_gene in b")
print(dim(b))
print("rownames(b)[1]"); print(rownames(b)[1])
print("look_at_gene: "); look_at_gene <- readline()
print("number of cells expressed this gene: "); print(number_of_cells_expressed_this_gene <- sum(as.data.frame(b[look_at_gene,]))) 
