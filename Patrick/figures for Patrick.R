source("Patrick load data 2016.R")
####general descriptions####
mean(rowSums(ferns.fatdf[(rowSums(ferns.fatdf))>0, (colSums(ferns.fatdf))>0])) #4.57
summary((rowSums(ferns.fatdf))==1)# 7 of 113 plots have only one species (none of which are singletons, I think)
summary((colSums(ferns.fatdf))==1)# 13 of the 54 (might include not-real species) species only occur in one place.

####richness figures####
#two choices: wait until env matches fernsfat, or make the totals by trimming
library(ggplot2)
rowSums(ferns.fatdf)
deleteme<-aggregate(rowSums(ferns.fatdf), by=list(sapply(strsplit(rownames(ferns.fatdf), split=" "), "[[",1)), FUN=mean)
fernpatches<-merge(fernpatches, deleteme, by.x=0, by.y=1)

tryrich.gplot<-ggplot(fernpatches, 
                      aes(x=Patch_size_ha, y=x, colour=Level_of_protection))+
  geom_point()+
  labs(x="Patch size (ha)", y="Mean transect richness")
tryrich.gplot

tryrich.gplot2<-ggplot(fernpatches, 
                      aes(x=Patch_size_ha, y=x, colour=Location_of_fragment))+
  geom_point()+
  labs(x="Patch size (ha)", y="Mean transect richness")
tryrich.gplot2
tryrich.gplot2+scale_x_log10()
ggsave("Patch size vs rich and location.pdf")

#is the number of plots roughly in proportion with the patch size?


####ordination analyses####
library(vegan)
dca.ferns<-decorana(ferns.fatdf[(rowSums(ferns.fatdf))>0, (colSums(ferns.fatdf))>1])#doesn't include species found in only one place. Has a rather long axis length.
plot(dca.ferns)
summary(dca.ferns)
nmds.ferns<-metaMDS(ferns.fatdf[(rowSums(ferns.fatdf))>0, (colSums(ferns.fatdf))>1])#doesn't converge. Either >1 or >0 occurrence species. Very noisy data or three axes. Sing

