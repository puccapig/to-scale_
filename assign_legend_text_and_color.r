#Rachel Yuan Nong 2020-OCT-20
#use to assign legend text and color
#2020-OCT-24 copied to Desktop/papercode/

#require factor labels.

#assign_legend_text_and_color <- function(factor_name){ 
	legend.by.input <- sapply(strsplit(factor_name, "_"), function(x){x[1]})
	cols.by.input <- rainbow(length(table(as.factor(legend.by.input))))[as.integer(as.factor(legend.by.input))]
	legend.by.input <- as.data.frame(legend.by.input)
	legend.by.input[,2] <- cols.by.input
	legend.by.input[,3] <- paste0(legend.by.input[,2], " ", legend.by.input[,1])
	legend.by.input <- unique(legend.by.input[,3])
	legend.by.input <- as.data.frame(legend.by.input)
	legend.by.input[,2] <- sapply(strsplit(as.character(legend.by.input[,1]), " "), function(x){x[1]})
	legend.by.input[,3] <- sapply(strsplit(as.character(legend.by.input[,1]), " "), function(x){x[2]})
	print("cols.by.input | legend.by.input")

	
