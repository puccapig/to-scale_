#Rachel Yuan Nong Mikkelsen 2022-JANUARY-08
#use scripts from or_any_sequence() and more footsteps

more_footsteps <- function(){
	print("read sequence from:")
	read_sequence_from <- readline()
	or_any_sequence <- read.delim2(read_sequence_from, header = FALSE, sep = "")
	len_seq <- nchar(as.character(or_any_sequence[2,1]))
	else_seq <- as.character(or_any_sequence[2,1])

	print("footstep_at_ :")
	footstep_at_ <- readline()
	footstep_at_ <- as.numeric(footstep_at_)

	bottle_cont <- 0
	for(i in 1:6){
		bottle_cont <- c(bottle_cont, (i*3) + footstep_at_)
		}
	bottle_cont <- bottle_cont[2:length(bottle_cont)]
	o_1 <- sample(bottle_cont, 6)
	#print(bottle_)
	print(o_1)

	par(mfrow = c(2,3), cex = 0.5)
	for(k in 1:6){
	footstep <- as.numeric(o_1[k])
	
	print(paste0("footstep: ", footstep))	
		chunk_seq <- round(len_seq/footstep)
	if(chunk_seq*footstep >= len_seq){chunk_seq <- chunk_seq}
	if(chunk_seq*footstep < len_seq){chunk_seq <- chunk_seq + 1}
	chart_seq <- matrix(0, ncol = 1, nrow = 4)
	chart_seq <- as.data.frame(chart_seq)
	rownames(chart_seq) <- c("A", "C", "G", "T")
	
	for(i in 1:chunk_seq){
		pos_start <- (i-1)*footstep + 1
		pos_stop <- pos_start + footstep - 1
		if(pos_stop > len_seq) { pos_stop <- len_seq }
		if(pos_stop <= len_seq) { pos_stop <- pos_stop }
		piece1 <- sapply(substr(else_seq, pos_start, pos_stop), function(x){x[1]})
		chart_piece1 <- matrix(0, nrow = 1, ncol = nchar(piece1))
		
		for(i in 1:dim(chart_piece1)[2]){
			chart_piece1[1,i] <- sapply(substr(piece1, i, i), function(x){x[1]})
			}
		table_chart_piece1 <- as.data.frame(table(chart_piece1[1,]))
		table_chart_piece1[,1] <- as.character(table_chart_piece1[,1])
		table_chart_piece1[,2] <- as.numeric(as.character(table_chart_piece1[,2]))
		
		piece_content <- matrix(0, ncol = 1, nrow = 4)
		rownames(piece_content) <- c("A", "C", "G", "T")
		for(ki in 1:dim(table_chart_piece1)[1]){
			base1 <- as.character(table_chart_piece1[ki,1])
			piece_content[base1, 1] <- table_chart_piece1[ki,2]/sum(table_chart_piece1[,2])
			}
		
	chart_seq <- cbind(chart_seq, piece_content)
	}
	chart_seq <- chart_seq[,2:dim(chart_seq)[2]]
	write.csv(chart_seq, file = "chart_seq.csv")
	print("chart_seq.csv")
	#heatmap(t(chart_seq), Rowv = NA, Colv = NA, col = cm.colors(256))
	#dev.new(); par(cex = 0.5)
	plot(c(1:dim(chart_seq)[2]), chart_seq[1,], pch = 19, col = "grey", ylim = c(0,1), ylab = c("proportion"), xlab = c("pos"))
	points(c(1:dim(chart_seq)[2]), chart_seq[2,], pch = 19, col = "red", ylim = c(0,1))
	points(c(1:dim(chart_seq)[2]), chart_seq[3,], pch = 19, col = "pink", ylim = c(0,1))
	points(c(1:dim(chart_seq)[2]), chart_seq[4,], pch = 19, col = "lightblue", ylim = c(0,1))
	legend("topright", legend = rownames(chart_seq), text.col = c("grey", "red", "pink", "lightblue"), bty = "n")
	title(sub = paste0("footstep", footstep, "nt | total", len_seq))  
}
}
		

