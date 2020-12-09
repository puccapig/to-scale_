#Rachel Yuan Nong 20190628 from (stanford) function.R.
#modified 2019-AUG-26, 2019-SEPT-01, 2019-OKT-16
#this is modified for looking at sum of raw counts per cell

print("Quit and assign new values to matrix_in")
print("function in use: col.from.target")
print("look_at_gene:"); look_at_gene <- readline()

library(RColorBrewer)
my.pal <- brewer.pal(9, "Set1")

target <- my.pal[1:length(look_at_gene)]
values <- matrix_in[match(look_at_gene, rownames(matrix_in), nomatch = 0),]

col.from.target <-
function(targets, values) {
      v <- (values-min(values))/(max(values)-min(values))
    targets.rgb <- col2rgb(targets)
    res <- vector("character", length=length(targets))
    for(i in 1:length(values)) {
      mytarget.rgb <- 255-t(unlist(apply(targets.rgb, 1, function(values) {(255-values) * v[i]})))
      res[i] <- rgb(red=mytarget.rgb[1]/256, green=mytarget.rgb[2]/256, blue=mytarget.rgb[3]/256)
    }
    return(res)
  }

cols.by.raw.counts <- col.from.target(target, values)

print("cols.by.raw.counts")
print(length(cols.by.raw.counts))
