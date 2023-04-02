#Rachel Yuan Nong Mikkelsen 2023APR02


unique_in_one_group <- function(){
print("list1_:")
list1_ <- readline()
list1_ <- read.csv(list1_)
list1_ <- as.character(list1_[,2])

print("list2_:")
list2_ <- readline()
list2_ <- read.csv(list2_)
list2_ <- as.character(list2_[,2])

list0_ <- c(list1_, list2_)
list0_ <- as.data.frame(table(list0_))
list0_ <- list0_[which(list0_[,2] == 1),1]
list0_ <- as.character(list0_)

print(">>>>>list0_")
}

print("unique_in_one_group")

