# needs labdsv
library(labdsv)
# community data
ferncomm<- read.table("D:\\rdata\\jennyferns\\fernspps.txt", row.names="X", sep= "\t", header=T)
# pull in grouping data, same as for betadist
trangrp<- read.table("D:\\rdata\\jennyferns\\transectgroupings.txt", row.names="transect", sep= "\t", header=T)
# run indval
indvalferns <- indval((ferncomm>1), trangrp$Forest, numitr=1000)
# to get summaries for all the species
indvalferns
# to only see those species with significant results
summary(indvalferns)

