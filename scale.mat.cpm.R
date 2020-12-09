#Rachel Yuan Nong 2019-07-01
#Stanford tumor.R function.R

norm.log.mat <- function(mat){
	     norm.fact <- colSums(mat)
	     mat.norm <- t(apply(mat, 1, function(x){x/norm.fact*1000000+1}))
	     print("dim(mat.norm)")
	     print(dim(mat.norm))
	     mat.log <- log2(mat.norm)
	     mat.log
}