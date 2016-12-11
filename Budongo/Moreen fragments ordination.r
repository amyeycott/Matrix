source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Budongo\\moreen fragments load data.r")
library(vegan)
dca21<-decorana(spp)#axis length 2.53
dca21
plot(dca21)
ca21<-cca(spp)
ca21
plot(ca21)#really quite horseshoey
dca7<-decorana(sppagg)# axis lenth 1.88
dca7
plot(dca7)


nmds21<-metaMDS(spp, k=2) #nice
nmds21
scores_nmds21<-scores(nmds21)
rownames(scores_nmds21)[substr(rownames(scores_nmds21),1,3)=="RP7"]<-c("N15.1", "N15.2", "N15.3")
x11(7,5, pointsize=9)
layout(matrix(c(1,2,3,4), ncol=2, nrow=2, byrow = TRUE), heights=c(2,1)) #first you say which cells of the layout are occupied by each graph. Eg c(1,1,0,2) in a 2x2 matrix makes a big figure in the top two cells and a small figure in the bottom right hand cell.
par(xpd=NA, mar=c(3,3,1,1), oma=c(0,0,0,0))
plot(nmds21,type="n", mgp=c(1.8, 0.5, 0), las=1, tcl=0.5)
points(scores_nmds21[-which(substr(rownames(scores(nmds21)),1,3)=="RP7"),],col= c("gray17","gray47","gray67")[as.factor(total$Matrix.type[-which(substr(total$Fragment.name,1,3)=="RP7")])], pch=16)
points(scores_nmds21[which(substr(rownames(scores(nmds21)),1,3)=="RP7"),],col= "black", pch=15)
text(scores_nmds21[,1],scores_nmds21[,2],labels=rownames(scores_nmds21),col= c("gray17","gray47","gray67", "black")[as.factor(total$Matrix.type)], pos=4)
legend(-1.0,-1.6, title="Matrix type", legend=c("Bushland","Farmland","Sugarcane","Continuous forest"),col=c("gray17","gray47","gray67", "black"), pch=c(16,16,16,15))
text(-1.3, 1.4,"a")
plot(nmds21, type="n", mgp=c(1.8, 0.5, 0), las=1, tcl=0.5)
points(nmds21,display="species", pch=c(1:10)[guildindex$Feeding.guild]) #guild index is a factor right now, factors are sorted alphabetically
legend(-1.0,-1.6, legend=levels(guildindex$Feeding.guild), pch=c(1:10), title="Feeding guild") # levels are also sorted alphabetically
text(-1.3, 1.4,"b")

what<-ordihull(nmds21,guildindex$Feeding.guild, display="species", draw="none")
lines(what$Frugivore)
lines(what$'Ground insectivore', lty=2)
#lines(what$Granivore, lty=3)
#lines(what$Nectarivore, lty=3)

savePlot("Moreen_nmdspair.emf", type="emf")
savePlot("Moreen_nmdspair.eps", type="eps")
#savePlot("Moreen_nmdspair_nolabels.emf", type="emf")
#savePlot("Moreen_nmdspair_nolabels.eps", type="eps")
