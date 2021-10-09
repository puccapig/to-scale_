#Rachel Yuan Nong Mikkelsen 2021-OCT-08
#count cuts by chromosomes |Tn5
#require 10xgenomics_scATAC_v1_E18_brain_fresh_5k from 10x..

#table_atac_mtx_e18_m_brain <- matrix(0, nrow = 1, ncol = 4008)
#table_atac_mtx_e18_m_brain <- as.data.frame(table_atac_mtx_e18_m_brain)
#a_chr <- as.character(unique(atac_mtx_e18_m_brain[,2]))
#a_chr <- a_chr[2:21]

#mtx_e <- mtx
#mtx_e <- as.matrix(mtx_e)
#mtx_e <- as.data.frame(mtx_e)
#mtx_e[, 4009] <- 0

table_atac_mtx_e18_m_brain <- matrix(0, nrow = 1, ncol = 4008)
table_atac_mtx_e18_m_brain <- as.data.frame(table_atac_mtx_e18_m_brain)
a_chr <- as.character(unique(atac_mtx_e18_m_brain[,2]))

k = 0
i = 0

for(k in 1:21){
        a <- mtx[atac_mtx_e18_m_brain[,2] == a_chr[k],]
	ai <- dim(a)[1]
	ai <- ai + 1
	a <- as.matrix(a)
	a <- as.data.frame(a)
	a[ai,] <- 0
	ai <- ai + 1
	a[ai,] <- 1 
        print(paste0("[FALSE 0] counting on a:", a_chr[k]))
for(i in 1:4008){
        #ai <- k + 1
        table_atac_mtx_e18_m_brain[k, i] <- table(a[,i] == 0)[[1]]
}
}

