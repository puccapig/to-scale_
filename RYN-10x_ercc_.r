#uploaded 2021-FEB-26
#Rachel Yuan Nong Mikkelsen 2019-11-08 Uppsala Sweden
#10x_ercc
#file ://cf.10xgenomics.com/samples/cell-exp/1.1.0/ercc/ercc_raw_gene_bc_matrices.tar.gz
#matrix loading script by 10x ://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/output/matrices

dim(ercc_raw_expt1_mass)
[1]     92 737280
hist(log2(colSums(ercc_raw_expt1_mass)+1))
