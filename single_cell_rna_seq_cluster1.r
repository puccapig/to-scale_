#Rachel Yuan Nong 2020-dec-09 Uppsala Sweden
#run on single cell RNA seq dataset

print("require cpm_mat")
print(dim(cpm_mat))
library(tsne)
pc_cpm_mat <- prcomp(cpm_mat)
pc_cpm_mat_a <- pc_cpm_mat[[2]]
my_tsne_pc_cpm_mat <- tsne(pc_cpm_mat_a[,1:10])
my_tsne_pc_cpm_mat_k3 <- tsne(pc_cpm_mat_a[,1:10], k = 3)

cutree_tsne <- cutree(hclust(dist(my_tsne_pc_cpm_mat_k3)), k = 15)

write.csv(my_tsne_pc_cpm_mat, file = "my_tsne.csv")
write.csv(cutree_tsne, file = "cluster1.csv")

print(" my_tsne.csv | cluster1.csv")
