# using beta dispersion function to see if the fern community is more variable in old or new, stream or distant transects

# community data
ferncomm<- read.table("D:\\rdata\\jennyferns\\fernspps.txt", row.names="X", sep= "\t", header=T)
# data that divides them by streamdist and by transect
trangrp<- read.table("D:\\rdata\\jennyferns\\transectgroupings.txt", row.names="transect", sep= "\t", header=T)
# library
library(vegan)

#check it went in ok, the first fern is asp mac
str(trangrp)
str(ferncomm)



beta.dist<- betadisper(vegdist(ferncomm),trangrp$distclass)
beta.dist
permutest(beta.dist, pairwise=TRUE)
beta.dist.HSD<- TukeyHSD(beta.dist)
plot(beta.dist.HSD)
plot(beta.dist)
boxplot(beta.dist)

beta.forest<- betadisper(vegdist(ferncomm),trangrp$Forest)
beta.forest
permutest(beta.forest, pairwise=TRUE)
beta.forest.HSD<- TukeyHSD(beta.forest)
plot(beta.forest.HSD)
plot(beta.forest)
boxplot(beta.forest)



# error messages and what they meant:
# "Error in x - c : non-conformable arrays" - either the grouping factor is not a factor or is named wrong

#documentation is at http://127.0.0.1:30020/library/vegan/html/betadisper.html