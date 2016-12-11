#Data loading & cleaning 2 - 44 in seedsnspores.r
#bits of this file with object creation/change (approx line nos): 1-79, 115-150, 232-237

#file splitting (step 1 find out what kinds of species are in seeds:
rowSums(table(thinseedsclean$Group, thinseedsclean$Species)>0)
#better to split in two stages 
thinseedstrees<-thinseedsclean[thinseedsclean$Group=="Tree",]
thinseedstrees$Species<-factor(thinseedstrees$Species)
thinseedsherbs<-thinseedsclean[thinseedsclean$Group!="Tree",]
thinseedsherbs$Species<-factor(thinseedsherbs$Species)

########Data manipulation and analysis begins here###############   (line 46 in seedsnspores.r)
herbseed.layers<-aggregate(thinseedsherbs[,6:7], by=list(thinseedsherbs$Transect), FUN=sum, na.rm=TRUE)
str(herbseed.layers)
rownames(herbseed.layers)<-herbseed.layers$Group.1
#want to include rows that are double zeros. The transects in use are all in thinseeds transect because that includes trays with contamination only.
treeseed.layers<-aggregate(thinseedstrees[,6:7], by=list(thinseedstrees$Transect), FUN=sum, na.rm=TRUE)
str(treeseed.layers)
rownames(treeseed.layers)<-treeseed.layers$Group.1
spore.layers<- aggregate(thinspores[,6:7], by=list(thinspores$Transect), FUN=sum, na.rm=TRUE)
str(spore.layers)
rownames(spore.layers)<-spore.layers$Group.1 
fill.zeros<-function(x){
  x1<-setdiff(unique(thinseedsclean$Transect), rownames(x))
  x2<-matrix(0, nrow=length(x1), ncol=ncol(x))
  rownames(x2)<-x1
  colnames(x2)<-colnames(x)
  x<-rbind(x,x2)
  x<-x[order(as.numeric(rownames(x))),]
  x
}
herbseed.layers<-fill.zeros(herbseed.layers)
treeseed.layers<-fill.zeros(treeseed.layers)
spore.layers<-fill.zeros(spore.layers)
    
# count by sample and depth


t.test(herbseed.layers$top1cm3, herbseed.layers$base4cm3, paired=T)
#t = 0.707, df = 32, p-value = 0.485 (seems to ignore both-zero)
#total area sampled - 39 transects, 10 samples, each of 2.6cm diameter. Each core is 5.31 cm2 and 26.55cm3. Each transect is 53.1cm2, total area is 2070.9cm2. Per transect volume is Top 1cm 53.1cm3, bottom  212.4cm3. total vol of all top layers is 2070.9cm3, bottom 4cm makes 8283.6cm3, total volume sampled 10354.5cm3 (about ten litres). 
herbseed.layers$denstop<-herbseed.layers$top1cm3/53.1
herbseed.layers$densbase<-herbseed.layers$base4cm3/212.4
herbseed.layers$totaldens<-herbseed.layers$denstop+herbseed.layers$densbase
herbseed.layers$perm2top<-(herbseed.layers$top1cm3/53.1)*(100*100)
herbseed.layers$perm2base<-(herbseed.layers$base4cm3/53.1)*(100*100)
herbseed.layers$perm2<-herbseed.layers$perm2top+herbseed.layers$perm2base
str(herbseed.layers)
t.test(herbseed.layers$denstop, herbseed.layers$densbase, paired=T, var.equal=F)#t = 3.477, df = 32, p-value = 0.001482. Default is that variance is unequal, but that doesn't help the skew issue
wilcox.test(herbseed.layers$denstop, herbseed.layers$densbase, paired=T)#V = 482, p-value = 0.0003123


#t.test(treeseed.layers$top1cm3, treeseed.layers$base4cm3, paired=T)# this is per unit area. Data may be too skew   
treeseed.layers$denstop<-treeseed.layers$top1cm3/53.1
treeseed.layers$densbase<-treeseed.layers$base4cm3/212.4
treeseed.layers$totaldens<-treeseed.layers$denstop+treeseed.layers$densbase
treeseed.layers$perm2top<-(treeseed.layers$top1cm3/53.1)*(100*100)
treeseed.layers$perm2base<-(treeseed.layers$base4cm3/53.1)*(100*100)
treeseed.layers$perm2<-treeseed.layers$perm2top+treeseed.layers$perm2base
str(treeseed.layers)
t.test(treeseed.layers$denstop, treeseed.layers$densbase, paired=T, var.equal=F)#t = 1.4834, df = 33, p-value = 0.1475. Default is that variance is unequal, but that doesn't help the skew issue
wilcox.test(treeseed.layers$denstop, treeseed.layers$densbase, paired=T) #V = 305, p-value = 0.2667

spore.layers$denstop<-spore.layers$top1cm3/53.1
spore.layers$densbase<-spore.layers$base4cm3/212.4
spore.layers$totaldensspore<- spore.layers$denstop+spore.layers$densbase
spore.layers$perm2top<-(spore.layers$top1cm3/53.1)*(100*100)
spore.layers$perm2base<-(spore.layers$base4cm3/53.1)*(100*100)
spore.layers$perm2<-spore.layers$perm2top+spore.layers$perm2base
str(spore.layers)
wilcox.test(spore.layers$denstop, spore.layers$densbase, paired=T)#V = 103, p-value = 0.01365

all.layers.dens<-merge(herbseed.layers, treeseed.layers[2:9], by=0, all=T)
rownames(all.layers.dens)<-all.layers.dens$Row.names
all.layers.dens<-merge(all.layers.dens,spore.layers[,2:9], by=0, all=T)
#all.layers.dens[is.na(all.layers.dens)]<-0  pretty sure this isn't necessary any more because of fill zeros function above
rownames(all.layers.dens)<-all.layers.dens$Row.names
all.layers.dens<-all.layers.dens[,4:27]
colnames(all.layers.dens)<-c("topsampleherb","basesampleherb","topdensityherb", "basedensityherb","totaldensherb","perm2topherb", "perm2baseherb", "totalperm2herb","topsampletree","basesampletree","topdensitytree", "basedensitytree","totaldenstree","perm2toptree", "perm2basetree", "totalperm2tree", "topsamplespore","basesamplespore","topdensityspore", "basedensityspore","totaldensspore","perm2topspore", "perm2basespore","totalperm2spore")

#forbarplot<-cbind(all.layers.dens$denstop.x, all.layers.dens$densbase.x, all.layers.dens$denstop.y, all.layers.dens$densbase.y)
#forbarplot<-as.data.frame(unclass(forbarplot))
#colnames(forbarplot)<-c("seeds 0-1cm", "seeds 1-5cm", "spores 0-1cm", "spores 1-5cm")
x11(); par(mar=c(9,5,1,1))
boxplot((all.layers.dens[,c(19,20,3,4,11,12)]), at=c(1,2,4,5,7,8),names=c("spores 0-1 cm", "spores 1-5 cm","herb/shrub seeds 0-1 cm", "herb/shrub seeds 1-5 cm", "tree seeds 0-1 cm", "tree seeds 1-5 cm"), ylab=expression(paste("Density cm"^-3)), cex.axis=0.75,  las=2)
savePlot("split-seed and spore density by layer.emf", type="emf")
#x11(); par(mar=c(8,4,1,1))
#boxplot(log((all.layers.dens[,c(3,4,11,12,19,20)])+1), names=c("herb/shrub seeds 0-1cm", "herb/shrub seeds 1-5cm", "tree seeds 0-1cm", "tree seeds 1-5cm","spores 0-1cm", "spores 1-5cm"), ylab="Log density cm-3", cex.axis=0.75,  las=2)  ### you really shouldn't log decimal numbers plus one...
#savePlot("split-seed and spore density by layer log.emf", type="emf")

#shapiro.test(all.layers.dens$topdensityherb)#W = 0.5812, p-value <0.001
#shapiro.test(all.layers.dens$basedensityherb) #W = 0.6841, p-value <0.001
#shapiro.test(forbarplot[,3]) #W = 0.3825, p-value = 1.796e-11
#shapiro.test(forbarplot[,4])  #  W = 0.823, p-value = 3.193e-05
#all not-normal - leptokutic skew, and lots of zeros for spores

#if the paired sample t-test compares the differences to zero, then to do an equivalent to levene we could do a shapiro on the differences?
shapiro.test((all.layers.dens$topdensityherb-all.layers.dens$basedensityherb)) #W = 0.7124, p-value <0.001
#shapiro.test((forbarplot[,3]-forbarplot[,4])) #W = 0.7569, p-value = 1.55e-06
#shapiro.test((forbarplot[,1]-forbarplot[,3])) #W = 0.5711, p-value = 2.375e-09
#shapiro.test((forbarplot[,2]-forbarplot[,4]))  #W = 0.9026, p-value = 0.003026
#it's a matrix so apply works (not eg sapply). The 2 tells it to go column by column, if you want row by row use 1.
apply(all.layers.dens, 2,function(x){shapiro.test(x)}) #all highly sig. But what I really want is to test it one column minus another for the t-tests above.
apply(all.layers.dens, 2,function(x){median(x)})
apply(all.layers.dens, 2,function(x){quantile(x, probs=c(0.25,0.75), na.rm=TRUE, type=7)})
colSums(all.layers.dens)
#median(forbarplot[,1]+forbarplot[,2])
#median(forbarplot[,3]+forbarplot[,4])
#quantile((forbarplot[,1]+forbarplot[,2]), probs=c(0.25,0.75), na.rm = TRUE, type = 7)
#quantile((forbarplot[,3]+forbarplot[,4]), probs=c(0.25,0.75), na.rm = TRUE, type = 7)

##taken from matrix veg load data because that one has contaminants in


herbseed.all<-xtabs(Total~Transect+Species, data=thinseedsherbs)
herbseed.all<-as.data.frame(unclass(herbseed.all))
herbseed.all<-herbseed.all[order(rownames(herbseed.all)),]
head(herbseed.all)
treeseed.all<-xtabs(Total~Transect+Species, data=thinseedstrees)
treeseed.all<-as.data.frame(unclass(treeseed.all))
treeseed.all<-treeseed.all[order(rownames(treeseed.all)),]
head(treeseed.all)
#have to remake spore.all to match, because in matrix veg load data it gets expanded to 74 lines
spore.all<-xtabs(Total~Transect+Species, data=thinspores)
spore.all<-as.data.frame(unclass(spore.all))
spore.all$DUMMY<-NULL
head(spore.all)
spore.all<-spore.all[order(rownames(spore.all)),]


#fill zeros is a function written further up
herbseed.all<-fill.zeros(herbseed.all)
treeseed.all<-fill.zeros(treeseed.all)
spore.all<-fill.zeros(spore.all)
# makes a data frame with the species abundances in ##Falls over because the new .all files do not have the same numbers of rows (samples) because of the empty ones.  Try adapting line 204 from load data. Also, spore.all from load data has dummy in it.
abunss<-sapply(list(spores=spore.all, herbseeds=herbseed.all, treeseeds=treeseed.all), function(x){
  rowSums(x)
})
abunss<-as.data.frame(abunss)
abunss
# makes a data frame with the species richnesses in
richss<-sapply(list(spores=spore.all, herbseeds=herbseed.all, treeseeds=treeseed.all), function(x){
  rowSums(x>0)
})
richss<-as.data.frame(richss)
richss

byage<-merge(abunss, richss, by=0)
byage<-merge(byage, env$Forest, by.x=1, by.y=0)
rownames(byage)<-byage$Row.names
#six plots by age
x11(point=16, height=6, width=6); par(mfrow=c(2,3), mar=c(2.5,2.5,2.5,0.5), font.main=1, cex.main =0.8, cex.axis=0.8)
boxplot(byage$spores.x~byage$y, main="Spore abundance ns", ylim=c(0,60), yaxt="n")
axis(side=2, at=c(0,20,40,60))
boxplot(byage$herbseeds.x~byage$y, main="Herb/shrub seed abundance ns", ylim=c(0,20))
boxplot(byage$treeseeds.x~byage$y, main="Tree seed abundance*", ylim=c(0,20))
boxplot(byage$spores.y~byage$y, main="Spore spp richness ns", ylim=c(0,5), yaxt="n")
axis(side=2, at=c(0,2,4))
boxplot(byage$herbseeds.y~byage$y, main="Herb/shrub seed spp richness ns", ylim=c(0,5), yaxt="n")
axis(side=2, at=c(0,2,4))
boxplot(byage$treeseeds.y~byage$y, main="Tree seed spp richness ns", ylim=c(0,5), yaxt="n")
axis(side=2, at=c(0,2,4))
savePlot("split seedsspores_by_age_pres.emf", type="emf")

library(car)
leveneTest(byage$herbseeds.x, byage$y)
leveneTest(byage$treeseeds.x, byage$y) 
leveneTest(byage$spores.x, byage$y)
leveneTest(byage$herbseeds.y, byage$y)
leveneTest(byage$treeseeds.y, byage$y)
leveneTest(byage$spores.y, byage$y)  #all of these pass levene test :-)
kruskal.test(byage$herbseeds.x, byage$y)  #abun Kruskal-Wallis chi-squared = 0.9838, df = 2, p-value = 0.6115
kruskal.test(byage$treeseeds.x, byage$y)  #Kruskal-Wallis chi-squared = 6.433, df = 2, p-value = 0.0401
kruskal.test(byage$spores.x, byage$y)     #Kruskal-Wallis chi-squared = 0.6378, df = 2, p-value = 0.727
kruskal.test(byage$herbseeds.y, byage$y)  #rich #Kruskal-Wallis chi-squared = 2.0416, df = 2, p-value = 0.3603
kruskal.test(byage$treeseeds.y, byage$y)  #Kruskal-Wallis chi-squared = 4.0162, df = 2, p-value = 0.1342
kruskal.test(byage$spores.y, byage$y)     #Kruskal-Wallis chi-squared = 0.743, df = 2, p-value = 0.6897

#x11(); par(mfrow=c(2,1))
#plot (jitter(abunss$spores, amount = 0.1)~jitter(abunss$seeds, amount=0.01), col=as.factor(env$Forest), xlab="abun_seeds", ylab="abun_spores")
#legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
#plot (jitter(richss$spores, amount = 0.1)~jitter(richss$seeds, amount=0.01), col=as.factor(env$Forest), xlab="spprichseeds", ylab="spprichspores")
#legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
#savePlot("pairs_seedsspores.emf")

cor.test(abunss$spores,abunss$herbseeds, method = "kendall")  #z = 0.5299, p-value = 0.5962
cor.test(abunss$spores,abunss$treeseeds, method = "kendall") #z = 1.8943, p-value = 0.05819
cor.test(abunss$herbseeds,abunss$treeseeds, method = "kendall") #z = 0.0763, p-value = 0.9392
cor.test(richss$spores, richss$herbseeds, method = "kendall")#z = 0.5984, p-value = 0.5496
cor.test(richss$spores, richss$treeseeds, method = "kendall") #z = 0.9, p-value = 0.3681
cor.test(richss$herbseeds, richss$treeseeds, method = "kendall") # z = -0.2416, p-value = 0.8091
cor.test(abunss$herbseeds, richss$herbseeds, method = "kendall") #***z = 6.5894, p-value = 4.415e-11
cor.test(abunss$treeseeds, richss$treeseeds, method = "kendall") #***z = 3.8309, p-value = 0.0001277
cor.test(abunss$spores, richss$spores, method = "kendall")#*** z = 5.0646, p-value = 4.093e-07

#x11();par(mfrow=c(2,2))
#plot(abunss$spores,abunss$seeds)
#plot(richss$spores, richss$seeds)
#plot(abunss$seeds, richss$seeds)
#plot(abunss$spores, richss$spores)

x11();par(mfrow=c(2,3))
mapply( function(x,n)hist(x, main= n),x=list(abunss$herbseeds, abunss$treeseeds, abunss$spores, richss$herbseeds, richss$treeseeds, richss$spores),n=c("herb seed abundance", "tree seed abundance", "spore abundance", "herb seed species richness","tree seed species richness","spore species richness"))
savePlot("split seedspore_hists.emf", type="emf")

#table(abunss$herbseeds)
#table(abunss$treeseeds)
#table(abunss$spores)

#sapply(list(abunss$herbseeds, abunss$treeseeds, abunss$spores, richss$herbseeds,richss$treeseeds, richss$spores), function(x){
#  sum(!is.na(x))
#})    #doesn't have na any more because I fixed it to have 39 samples

sapply(list(abunss$herbseeds, abunss$treeseeds, abunss$spores, richss$herbseeds,richss$treeseeds, richss$spores), function(x){
  table((x))
})


sort(colSums(herbseed.all,na.rm=T))
#  5 most abundant species: Crassocephalum vitellinum 6, Piper capensis 20, Pseuderanthemum ludovicianum 16, Synedrella nodiflora  6, Celosia trigyna/Crassocephalum montuosum/Kyllinga brevifolia/Phyllanthus amarus  all have 3
sort(colSums(treeseed.all,na.rm=T)
#5 most abundant tree species (but there are only 5): Broussonetia papyrifera 125, Ficus sur 13, Ficus asperifolia 2, Uncaria africana 2, Harungana madagascariensis 1 
sort(colSums(herbseed.all>0,na.rm=T))
#5 most frequent species (&n sites): Celosia trigyna 3, Crassocephalum vitellinum 5, Synedrella nodiflora 6, Pseuderanthemum ludovicianum 7, Piper capensis 9
sort(colSums(treeseed.all>0,na.rm=T))
# 5 most frequent species (&n sites) : Ficus asperifolia 1, Harungana madagascariensis 1, Uncaria africana 2, Ficus sur 7, Broussonetia papyrifera 32 
sort(colSums(spore.all,na.rm=T))
#there are only 5 species. In order of abundance: Christella parasitica 578, Microlepia specuncea 20, Pellaea viridis 8, Doryopteris concolor 1, Amauropelta oppositiformis 1
sort(colSums(spore.all>0,na.rm=T))
#Same order, 31 9 6 1 1 samples respectively

floorveg<-merge(trueherbs, herbshrubs, by.x=0, by.y=0)
floorveg$Row.names<-as.factor(floorveg$Row.names)
rownames(floorveg)<-floorveg$Row.names
floorveg<-floorveg[2:46]
rowSums(floorveg)
### there is one empty row, transect 74 was just big trees

sort(colSums(floorveg>0, na.rm=T)) # Dracaena fragrans 25, Leptaspis zeylanica 25, Argomuellera macrophylla 25, Pseuderanthemum ludovicianum 26, Acalypha neptunica 33
sort(colSums(floorveg/35, na.rm=T)) # Argomuellera macrophylla 2.95, Leptaspis zeylanica 5.66, Dracaena fragrans 9.98, Pseuderanthemum ludovicianum 12.79,  Acalypha neptunica 13.93  - 35 is the n transects, so gives av cover
sort(colSums(ferns, na.rm=T)) #Pteris catoptera 34, Pteris atrovirens/burtonii 38, Asplenium unilaterale  39, Pteris hamulosa 65,  Asplenium macrophlebium 108
sort(colSums(ferns>0, na.rm=T)) #Arthopteris palisoti 17, Christella parasitica 22, Pteris atrovirens/burtonii 23, Pteris hamulosa 30,  Asplenium macrophlebium 32


#need to decide whether to use big trees, saplings, or both. This uses big trees.
sort(colSums(trees, na.rm=T)) #Broussonetia papyrifera 28, Celtis wightii  28, Funtumia elastica 32, Celtis durandii 36 Celtis mildbreadii 36
sort(colSums(trees>0, na.rm=T)) #Celtis wightii 12, Celtis mildbreadii 15, Celtis durandii 16, Diospyros abyssinica 16, Funtumia elastica 16

###species accumulation
library(vegan)
keep1<-!is.na(floorveg[,1]) 
keep3c<-!is.na(ferns[,1])
keep7<-!is.na(herbseed.all[,1]) #same one can be used for herbtree
keep9<-!is.na(spore.all[,1])
keep5c<-!is.na(trees[,1])

spaccfloorveg <- specaccum(floorveg[keep1,], "exact")
spaccfern <- specaccum(ferns[keep3c,], "exact")
spaccherbseed <- specaccum(herbseed.all[keep7,], "exact")
spacctreeseed<-specaccum(treeseed.all[keep7,], "exact")
spacctree<-specaccum(trees[keep5c,], "exact")
spaccspore <- specaccum(spore.all[keep9,], "exact")
spaccspore.df<-cbind(spaccspore$sites, spaccspore$richness, spaccspore$sd)  

#sample size per veg plot was 150m2 #the log in the first plot makes all the subsequent lines plot loggy too
plot((spaccfern$sites*300),spaccfern$richness, col="black", type="l", ylab="cumulative species richness",xlab=expression(paste("area sampled m"^2)), log="xy", xlim=c(0.001,20000), ylim=c(1,50), lwd=2)          #ylab=expression(paste("Density cm"^-3))
lines((spaccfloorveg$sites*150),spaccfloorveg$richness, col="black", lty=2, lwd=2)
lines((spacctree$sites*300),spacctree$richness, col="black", lty=3, lwd=2)
lines((spaccspore$sites*0.00531),spaccspore$richness, col="grey36", lwd=2)
lines((spaccherbseed$sites*0.00531),spaccherbseed$richness, col="grey36", lty=2, lwd=2)
lines((spacctreeseed$sites*0.00531),spacctreeseed$richness, col="grey36", lty=3, lwd=2)

                    
legend("bottomright",c("Ferns","Herbs/shrubs","Trees", "Spores","Herb/shrub seeds","Tree seeds"),lty=c(1,2,3,1,2,3),col=c("black","black","black","grey36","grey36", "grey36"), lwd=2)
savePlot("split sppacc loglog ie species area.emf", type="emf")


plot(spaccfloorveg, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(1,70), xaxs="i", ylim=c(0,50), yaxs="i", bty="l", ylab="")  
plot(spacctree, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
plot(spaccfern, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(spaccherbseed, add=T, ci.type="line", col="navy", lwd=2, ci.lty=1, ci.col="lightblue")
plot(spacctreeseed, add=T, ci.type="line", col="purple", lwd=2, ci.lty=1, ci.col="lavender")
plot(spaccspore, add=T, ci.type="line", col="gold", lwd=2, ci.lty=1, ci.col="palegoldenrod")
legend("topright",c("Herbs/shrubs","Trees","Ferns","Herb/shrub seeds","Tree seeds", "Spores"),pch=16,col=c("blue","red","green","navy","purple", "gold"))
savePlot("seedbank accumulation curves_bright colours.emf", type="emf")       

###ordinations - we kept the nmds



#dca.subseta<-seed.all[!is.na(rowSums(seed.all)),]
#row 6 site 10 (OG) and row 17 site 36 (OS) are absolute outliers, ie a completely different composition to all the others. NMDS doesn't really cure this, so removing sites must be the way to go. 
#dca.subsetb<-dca.subseta[!rownames(dca.subseta) %in% c(10,36),]
#seed.dca<-decorana(dca.subsetb)
#summary(seed.dca)
#x11();par(mfrow=c(1,1))
#plot(seed.dca)
#seed.ca<-cca(dca.subsetb)
#plot(seed.ca) #yuck. Stick with DCA which is a nice plot once the outliers are removed.
 
#new.subset<-dca.subseta[!rownames(dca.subseta) %in% c(10,36),colSums(dca.subseta>0)>2]
#seed.ca<-cca(new.subset)
#plot(seed.ca)

#seed.ca.age<-cca(new.subset~y, data=byage[!rownames(byage) %in% c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),])
#plot(seed.ca.age, type="n")
#points(seed.ca.age, disp="sites", col=byage[!rownames(byage) %in% c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),]$y)
#anova(seed.ca.age)

# the nmds can either be done on only those sites where all the kinds of data exist (27) so tht anything can be procrustied to anything else, or it can be done on the max dataset for that dataset, but that will probably makes procrustes crash but will give more power for testing age effects. The ferns, trees and veg can't be mushed together because they were recorded in different ways. Does that mean the procrustes between them would be invalid?
nmds.herbseeds<-metaMDS(herbseed.all[(keep7&(rowSums(herbseed.all)>0)),],k=2)# several empty sites
herbseed.sitescores<-as.data.frame(scores(nmds.herbseeds, choice=1:2, disp="sites"))
colnames(herbseed.sitescores)<-c("herbseed.ax1", "herbseed.ax2")
nmds.treeseeds<-metaMDS(treeseed.all[(keep7&(rowSums(treeseed.all)>0)),],k=2)
treeseed.sitescores<-scores(nmds.treeseeds, choice=1:2, disp="sites")
colnames(treeseed.sitescores)<-c("treeseed.ax1", "treeseed.ax2")
nmds.spores<-metaMDS(spore.all[(keep9&(rowSums(spore.all)>0)),],k=2)
spore.sitescores<-scores(nmds.spores, choice=1:2, disp="sites")
colnames(spore.sitescores)<-c("spore.ax1", "spore.ax2")
nmds.floorveg<-metaMDS(floorveg[(keep1&(rowSums(floorveg)>0)),],k=2)
floorveg.sitescores<-scores(nmds.floorveg, choice=1:2, disp="sites")
colnames(floorveg.sitescores)<-c("floorveg.ax1", "floorveg.ax2")
nmds.trees<-metaMDS(trees[(keep5c&(rowSums(trees)>0)),],k=2)
trees.sitescores<-scores(nmds.trees, choice=1:2, disp="sites")
colnames(trees.sitescores)<-c("trees.ax1", "trees.ax2")
nmds.ferns<-metaMDS(ferns[(keep3c&(rowSums(ferns)>0)),],k=2)
fern.sitescores<-scores(nmds.ferns, choice=1:2, disp="sites")
colnames(fern.sitescores)<-c("ferns.ax1", "ferns.ax2")


splitscores<-merge(env[2],herbseed.sitescores, by.x=0, by.y=0, all=T)
splitscores<-merge(splitscores,treeseed.sitescores, by.x=1, by.y=0, all=T)
splitscores<-merge(splitscores,spore.sitescores, by.x=1, by.y=0, all=T)
splitscores<-merge(splitscores,floorveg.sitescores, by.x=1, by.y=0, all=T)
splitscores<-merge(splitscores,trees.sitescores, by.x=1, by.y=0, all=T)
splitscores<-merge(splitscores,fern.sitescores, by.x=1, by.y=0, all=T)
rownames(splitscores)<-splitscores$Row.names

apply(splitscores[,3:14],2,function(x){kruskal.test(x[!is.na(x)], splitscores$Forest[!is.na(x)])})
# sig   spores, ferns 

#THIS IS GOOD:
nmds.herbseeds.fullset<-metaMDS(herbseed.all[(rowSums(herbseed.all)>0),],k=2)# several empty sites
herbseed.fullset.sitescores<-as.data.frame(scores(nmds.herbseeds, choice=1:2, disp="sites"))
colnames(herbseed.fullset.sitescores)<-c("herbseed.ax1", "herbseed.ax2")
envfit(nmds.herbseeds.fullset,byage[rownames(byage) %in%  rownames (herbseed.fullset.sitescores),], permutations=999, display="sites", na.rm=T)  


x11();par(mfrow=c(2,3))
plot(splitscores$herbseed.ax2[!is.na(splitscores$herbseed.ax2)]~splitscores$herbseed.ax1[!is.na(splitscores$herbseed.ax1)], col=as.factor(splitscores$Forest[!is.na(splitscores$herbseed.ax1)]))#pretty bad - six or so outliers
plot(splitscores$treeseed.ax2[!is.na(splitscores$treeseed.ax2)]~splitscores$treeseed.ax1[!is.na(splitscores$treeseed.ax1)], col=as.factor(splitscores$Forest[!is.na(splitscores$treeseed.ax1)]))#really quite dreadful - max axis 1 score is 300, next highest is 10
plot(splitscores$spore.ax2[!is.na(splitscores$spore.ax2)]~splitscores$spore.ax1[!is.na(splitscores$spore.ax1)], col=as.factor(splitscores$Forest[!is.na(splitscores$spore.ax1)])) # fine - how is it managing to be fine on two axes with such little information???
plot(splitscores$floorveg.ax2[!is.na(splitscores$floorveg.ax2)]~splitscores$floorveg.ax1[!is.na(splitscores$floorveg.ax1)], col=as.factor(splitscores$Forest[!is.na(splitscores$floorveg.ax1)]))#very nice plot
plot(splitscores$trees.ax2[!is.na(splitscores$trees.ax2)]~splitscores$trees.ax1[!is.na(splitscores$trees.ax1)], col=as.factor(splitscores$Forest[!is.na(splitscores$trees.ax1)]))# also just fine
plot(splitscores$ferns.ax2[!is.na(splitscores$ferns.ax2)]~splitscores$ferns.ax1[!is.na(splitscores$ferns.ax1)], col=as.factor(splitscores$Forest[!is.na(splitscores$ferns.ax1)])) #one crazy outlier, the rest are fine and an age effect is plausible


#sco.nmds1<-scores(nmds1, choice=1:2, disp="sites")
#spp.nmds1<-scores(nmds1, choice=1:2, disp="species")
#plot(nmds1, type="n", cex=1.4) 
#points(nmds1, display="sites", col=c(2,3,4)[byage[!rownames(byage) %in% c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),]$y], pch=16)
#legend("bottomright", legend=levels(byage$y), col=c(2,3,4), pch=16, cex=1.4)
#savePlot("seedbank NMDS_bright colours.emf") 

#envfit(nmds1,byage[!rownames(byage) %in%  c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),][,"y",drop=F], permutations=999, display="sites", na.rm=T)  

#procrustes(nmds1, seed.dca, symmetric=T) #0.5287  so not so bad...

#sco.dcaseeds<-scores(seed.dca, choices=1:2, disp="sites")
#sco.dcaseeds<-merge(sco.dcaseeds, env[2], by.x=0, by.y=0)
#sco.dcaseeds<-merge(sco.dcaseeds, sco.nmds1, by.x=1, by.y=0) 
#leveneTest(sco.dcaseeds$DCA1, sco.dcaseeds$Forest)
#leveneTest(sco.dcaseeds$DCA2, sco.dcaseeds$Forest)
#leveneTest(sco.dcaseeds$NMDS1, sco.dcaseeds$Forest)
#leveneTest(sco.dcaseeds$NMDS2, sco.dcaseeds$Forest)
#all of these pass levene test

#however you do the test, the following relationships are ns
#kruskal.test(sco.dcaseeds$DCA1, sco.dcaseeds$Forest)
#lm.DCA1<-lm(sco.dcaseeds$DCA1~sco.dcaseeds$Forest)
#summary(lm.DCA1)
#kruskal.test(sco.dcaseeds$DCA2, sco.dcaseeds$Forest)
#lm.DCA2<-lm(sco.dcaseeds$DCA2~sco.dcaseeds$Forest)
#summary(lm.DCA2)
#kruskal.test(sco.dcaseeds$NMDS1, sco.dcaseeds$Forest)
#lm.NMDS1<-lm(sco.dcaseeds$NMDS1~sco.dcaseeds$Forest)
#summary(lm.NMDS1)
#kruskal.test(sco.dcaseeds$NMDS2, sco.dcaseeds$Forest)
#lm.NMDS2<-lm(sco.dcaseeds$NMDS2~sco.dcaseeds$Forest)
#summary(lm.NMDS2)

######## similarity
#which plots have which data?
datss<-data.frame(spores=1:74%in%rownames(spore.all),seeds=1:74%in%rownames(herbseed.all),fernplots=1:74%in%rownames(na.omit(ferns)), vegplots=1:74%in%rownames(na.omit(herbshrubs)))
table (datss[2:4])

#which how many of these plots have species in common?




library(analogue)
 
dim(herbseed.all)
dim(floorveg)
herbseed74<-fill.na(herbseed.all)#function borrowed from matrix veg load data. Check the rownames are in the same order, then go backwards, pulling out matched sites only
rownames(herbseed74)
rownames(floorveg)
head(herbseed74)
head(floorveg)
rownames(floorveg)<-as.factor(rownames(floorveg))
floorveg<-floorveg[order(rownames(floorveg)),]
rownames(herbseed74)<-as.factor(rownames(herbseed74))
herbseed74<-herbseed74[order(rownames(herbseed74)),]
herbseed.red<-herbseed74[!is.na(herbseed74[,1])&!is.na(floorveg[,1]),] 
floorveg.red<-floorveg[!is.na(herbseed74[,1])&!is.na(floorveg[,1]),]
dim(herbseed.red)
dim(floorveg.red)

treeseed74<-fill.na(treeseed.all)
head(treeseed74)
head(trees)    #order matches
treeseed.red<-treeseed74[!is.na(treeseed74[,1])&!is.na(trees[,1]),]
trees.red<-trees[!is.na(treeseed74[,1])&!is.na(trees[,1]),]

spore74<-fill.na(spore.all)
head(spore74)
head(ferns)#order is right
spores.red<-spore74[!is.na(spore74[,1])&!is.na(ferns[,1]),]
ferns.red<-ferns[!is.na(spore74[,1])&!is.na(ferns[,1]),]

commonspp.herbsnseeds<-intersect(colnames(herbseed74), colnames(floorveg.red)) #only 3 species in both datasets?
fun.commonspp<-function (a,b)((a>0)&(b>0))
commonmatrix.herbsnseeds<-mapply(fun.commonspp, a=herbseed.red[,commonspp.herbsnseeds], b=floorveg.red[,commonspp.herbsnseeds])  #nb separate out the individual bits after the = sign to check the common names are in the same order. Only works without NAs
sum(apply(commonmatrix.herbsnseeds,1,any)==T) #4

commonspp.treesnseeds<-intersect(colnames(treeseed74), colnames(trees)) # 3 species in both datasets
commonmatrix.treesnseeds<-mapply(fun.commonspp, a=treeseed.red[,commonspp.treesnseeds], b=trees.red[,commonspp.treesnseeds])  #separate out the individual bits after the = sign to check the common names are in the same order.  
sum(apply(commonmatrix.treesnseeds,1,any)==T)

commonspp.fernsnspores<-intersect(colnames(spore.all), colnames(ferns)) # Only C parasitica 
commonmatrix.fernsnspores<-mapply(fun.commonspp, a=spores.red[,commonspp.fernsnspores], b=ferns.red[,commonspp.fernsnspores])  #separate out the individual bits after the = sign to check the common names are in the same order.  
sum(apply(commonmatrix.fernsnspores,1,any)==T)  #falls over because there is only one species


#herbsnseeds<-join(herbseed.red, floorveg.red, split=F)
#herbsnseeds<-cbind(seed.plant=factor(c(rep("seed", nrow(herbseed.red)) ,rep("plant",nrow(floorveg.red)))), herbsnseeds)     
#dim(herbsnseeds)
#vegdistmat<-vegdist(herbsnseeds[,2:67], method="bray", na.rm=T) # empty rows look as if they might get troublesome - this is because levels don't get dropped in the reduced datasets, easy enough to fix
#vegdistmat<-as.matrix(vegdistmat)#stopped here and moved onto martaplot/Ingerplot




#seedsfordist1<-seed.all
#rownames(seedsfordist1)<-as.factor(rownames(seedsfordist1))
#seedsfordist1<-seedsfordist1[order(rownames(seedsfordist1)),]
#seedsfordist1
#vegfordist1<-veg
#vegfordist<-vegfordist1[(!is.na(vegfordist1[,1])&!is.na(seedsfordist1[,1])),]
#seedsfordist<-seedsfordist1[(!is.na(vegfordist1[,1])&!is.na(seedsfordist1[,1])),]


#allt<-join(seedsfordist, vegfordist, split=F)
#allt<-cbind(seed.plant=factor(c(rep("seed", nrow(seedsfordist)) ,rep("plant",nrow(vegfordist)))), allt)

#vegdistmat<-vegdist(allt[,2:153], method="bray", na.rm=T) 
#vegdistmat<-as.matrix(vegdistmat)
#distance.values<-c(vegdistmat[1,27],vegdistmat[2,28],vegdistmat[3,29],vegdistmat[4,30],vegdistmat[5,31],vegdistmat[6,32],vegdistmat[7,33],vegdistmat[8,34],vegdistmat[9,35],vegdistmat[10,36],vegdistmat[11,37],vegdistmat[12,38],vegdistmat[13,39],vegdistmat[14,40],vegdistmat[15,41],vegdistmat[16,42],vegdistmat[17,43],vegdistmat[18,44],vegdistmat[19,45],vegdistmat[20,46],vegdistmat[21,47],vegdistmat[22,48],vegdistmat[23,49],vegdistmat[24,50],vegdistmat[25,51],vegdistmat[26,52])
#distance.transects<-c(1,15,17,20,21,24,26,27,3,36,38,39,4,45,47,48,57,59,6,60,62,66,68,71,72,74) 
#distances<-cbind(distance.transects, distance.values)
#distances2<-merge(distances,env, by.x=1, by.y=0)
#kruskal.test(distances2$distance.values, distances2$Forest)
#tapply(distances2$distance.values, distances2$Forest, FUN=median)

#sort(names(all))

####### Inger plots
#allsums<-by(allt[,2:153], allt$seed.plant, colSums, simplify=F)
#allsums<-as.data.frame(cbind(plant=allsums$plant, seed=allsums$seed)) 
#allsums<-allsums[(allsums$plant+allsums$seed)>0,] 
#allsums<-allsums[order(allsums$plant, allsums$seed),]  #this likes a df
#barplot(as.matrix(log(allsums+1)), beside=T, horiz=T, xlim=c(-1, 7), xlab="Log total abundance", col="white", yaxp=c(0, 40, 20), space=c(0,3)) #this likes a matrix
#savePlot("Martaplot seeds vs veg.emf")     


herbnseed74<-join(herbseed74, floorveg, split=F)
herbnseed74<-cbind(seed.plant=factor(c(rep("seed", nrow(herbseed74)) ,rep("plant",nrow(floorveg)))), herbnseed74)
herbnseed.sums<-by(herbnseed74[,2:67], herbnseed74$seed.plant, colSums, simplify=F)
herbnseed.sums<-as.data.frame(cbind(plant=herbnseed.sums$plant, seed=herbnseed.sums$seed))
herbnseed.sums$seedppn<-herbnseed.sums$seed/sum(herbnseed.sums$seed)
herbnseed.sums$vegppn<-herbnseed.sums$plant/sum(herbnseed.sums$plant)
herbnseed.sums$ordering<-ifelse(herbnseed.sums$seed!=0&herbnseed.sums$plant==0,1,(ifelse(herbnseed.sums$seed!=0&herbnseed.sums$plant!=0,2,3)))
herbnseed.sums<-herbnseed.sums[order(herbnseed.sums$ordering,herbnseed.sums$seed, herbnseed.sums$plant),]  #this likes a df
treenseed<-join(treeseed.all, trees, split=F)
treenseed<-cbind(seed.tree=factor(c(rep("seed", nrow(treeseed.all)) ,rep("plant",nrow(trees)))), treenseed)
treenseed.sums<-by(treenseed[,2:53], treenseed$seed.tree, colSums, simplify=F)
treenseed.sums<-as.data.frame(cbind(plant=treenseed.sums$plant, seed=treenseed.sums$seed))
treenseed.sums$seedppn<-treenseed.sums$seed/sum(treenseed.sums$seed)
treenseed.sums$treesppn<-treenseed.sums$plant/sum(treenseed.sums$plant)
treenseed.sums$ordering<-ifelse(treenseed.sums$seed!=0&treenseed.sums$plant==0,1,(ifelse(treenseed.sums$seed!=0&treenseed.sums$plant!=0,2,3)))
treenseed.sums<-treenseed.sums[order(treenseed.sums$ordering,treenseed.sums$seed, treenseed.sums$plant),]  #this likes a df
sporefern<-join(spore.all, ferns, split=F)
sporefern<-cbind(spore.plant=factor(c(rep("spore", nrow(spore.all)) ,rep("plant",nrow(ferns)))), sporefern)
sporefernsums<-by(sporefern[2:34],sporefern$spore.plant, colSums, simplify=F)
sporefernsums<-as.data.frame(cbind(plant=sporefernsums$plant,spore=sporefernsums$spore)) 
sporefernsums$sporeppn<-sporefernsums$spore/sum(sporefernsums$spore)
sporefernsums$plantppn<-sporefernsums$plant/sum(sporefernsums$plant)
sporefernsums$ordering<-ifelse(sporefernsums$spore!=0&sporefernsums$plant==0,1,(ifelse(sporefernsums$spore!=0&sporefernsums$plant!=0,2,3)))
sporefernsums <-sporefernsums[order(sporefernsums$ordering,sporefernsums$sporeppn,sporefernsums$plantppn),]

x11()
barplot(t(as.matrix(herbnseed.sums[,c(3,4)])), beside=T, horiz=T,  xlab="Proportion of total", xlim=c(0,0.3), yaxs="i", las=2, cex.names=0.7, main="Herb and shrub species") #make the biggest graph
ylim0<-par()$usr[3:4] #find out how long the axis is and make that the axis length for all subsequent graphs. 
x11();par(mar=c(4,13,3,1), cex=0.7)
barplot(t(as.matrix(herbnseed.sums[,c(3,4)])), beside=T, horiz=T,  xlab="Percentage of total", xlim=c(0,0.3),ylim=ylim0, xaxt = "n",las=2, font.axis = 3, cex.names=0.8) #this likes a matrix. x is supressed so that y (the trickier axis to draw manually) can be in italics
colour=c("grey88","grey25")
legend(x=0.2, y=175, legend=c("in vegetation (% cover)","in seedbank"), fill=colour)
#locator(type = "n")#find where the title should go
mtext(side=3,"Herb and shrub species")
axis(1, font.axis=1, labels=c(0,10,10,30), at=c(0,0.1,0.2,0.3)) #puts the x axis in without italics and turns it into %
savePlot("split herbsnshrubs ingerplot.png", type="png") 
# only 3 shared species:  Piper capensis, Pseuderanthemum ludovicianum, Solanum mauritianum. Note that ferns and trees have 4 and 3 shared species respectively.  

barplot(t(as.matrix(treenseed.sums[,c(3,4)])),beside=T, horiz=T,  xlab="Percentage of total", xlim=c(0,0.3),ylim=ylim0, xaxt="n", las=2, font.axis=3, cex.names=0.8) 
legend(x=0.2, y=150, legend=c("in vegetation (abundance)","in seedbank"), fill=colour) 
#locator(type = "n")
text(0.1,165 ,"Tree species", cex=1.3)
text(0.29,20,round(max(treenseed.sums[,c(3,4)])*100, digits=2))
axis(1, font.axis=1, labels=c(0,10,10,30), at=c(0,0.1,0.2,0.3))
savePlot("split treesseeds ingerplot.png", type="png")
 
barplot(t(as.matrix(sporefernsums[,c(3,4)])), beside=T, horiz=T,  xlab="Percentage of total", xlim=c(0,0.3), ylim=ylim0, xaxt="n", las=2, font.axis=3, cex.names=0.8)
legend(x=0.16, y=95, legend=c("in vegetation (relative abundance)","in seedbank"), fill=colour)
#locator(type = "n")
text(0.1,105,"Fern species", cex=1.3) 
text(0.29,20,round(max(sporefernsums[,c(3,4)])*100, digits=2))
axis(1, font.axis=1, labels=c(0,10,10,30), at=c(0,0.1,0.2,0.3))
savePlot("split sporesnferns ingerplot.png", type="png") 

### Flora Tropical East Africa classificat
str(seed.all)
str(thinseeds)
levels(thinseeds$Group)
Grasslist<-thinseeds[thinseeds$Group=="Grass",3]
Grasslist<-factor(Grasslist)
seedsgrasses<-seed.all[,which(names(seed.all) %in% Grasslist)]
Nonwoodylist<-thinseeds[thinseeds$Group=="Nonwoody",3]
Nonwoodylist<-factor(Nonwoodylist)
seedsNonwoody<-seed.all[,which(names(seed.all) %in% Nonwoodylist)]
Shrublist<-thinseeds[thinseeds$Group=="Shrub",3]
Shrublist<-factor(Shrublist)
seedsShrubs<-seed.all[,which(names(seed.all) %in% Shrublist)]
Treelist<-thinseeds[thinseeds$Group=="Tree",3]
Treelist<-factor(Treelist)
seedsTrees<-seed.all[,which(names(seed.all) %in% Treelist)]

seedsgrass_forhabit<-merge(seedsgrasses, env, by=0)
seedsgrass_forhabit<-seedsgrass_forhabit[!is.na(seedsgrass_forhabit$Forest),]
seedsgrass_forhabit<-rowSums(rowsum(seedsgrass_forhabit[2:6],seedsgrass_forhabit$Forest, na.rm=T))
seedsShrub_forhabit<-merge(seedsShrubs, env, by=0)
seedsShrub_forhabit<-seedsShrub_forhabit[!is.na(seedsShrub_forhabit$Forest),]
seedsShrub_forhabit<-rowSums(rowsum(seedsShrub_forhabit[2:3],seedsShrub_forhabit$Forest, na.rm=T))
seedsNonwoody_forhabit<-merge(seedsNonwoody, env, by=0)
seedsNonwoody_forhabit<-seedsNonwoody_forhabit[!is.na(seedsNonwoody_forhabit$Forest),]
seedNonwoody_forhabit<-rowSums(rowsum(seedsNonwoody_forhabit[2:18],seedsNonwoody_forhabit$Forest, na.rm=T))
seedsTrees_forhabit<-merge(seedsTrees, env, by=0)
seedsTrees_forhabit<-seedsTrees_forhabit[!is.na(seedsTrees_forhabit$Forest),]
seedTrees_forhabit<-rowSums(rowsum(seedsTrees_forhabit[2:6],seedsTrees_forhabit$Forest, na.rm=T))
sums_forhabit<-cbind(seedsgrass_forhabit, seedsShrub_forhabit, seedNonwoody_forhabit,  seedTrees_forhabit)
colSums(rowSums(sums_forhabit))
(sums_forhabit)/(rowSums(sums_forhabit))*100
colSums(sums_forhabit)/sum(colSums(sums_forhabit))*100

########TRAITS########## this can be started just from line 43 of seedsnspores
fortraits<-aggregate(thinseedsclean[,6:8], by=list(thinseedsclean$Species), FUN=sum, na.rm=TRUE)
con2<-odbcConnectExcel("Seedbank Datasheet_corrected.xls2.xls")
sqlTables(con2)
fortraits2<-sqlQuery(con2, "Select * From [traits_r$]")
close(con2)
fortraits<-merge(fortraits, fortraits2, by=1)

table(fortraits$Dispersal)# nspp
listy<-by(fortraits$Total,fortraits$Dispersal, FUN=sum) #nseeds
listy/(sum(fortraits$Total))

listy2<-by(fortraits$Total[fortraits$Lhist!="tree"],fortraits$Lhist[fortraits$Lhist!="tree"], FUN=sum) #nseeds
listy2/(sum(fortraits$Total[fortraits$Lhist!="tree"]))


kruskal.test(fortraits$Total, fortraits$Dispersal)#Kruskal-Wallis chi-squared = 4.204, df = 5, p-value = 0.5204
x11(); par(mar=c(8,4,1,1))
boxplot(log(fortraits$Total)~fortraits$Dispersal,las=3, ylab="Log total seeds")
savePlot("seed dispersal vs abundance.emf", type="emf")
pch.list<-as.numeric(fortraits$Dispersal)
plot(log(fortraits$Total)~fortraits$seed_size_mm, pch=as.numeric(fortraits$Dispersal), ylim=c(0,3), xlab="Seed size (mm)", ylab="log (total seeds +1)")
legend("topleft", legend=unique(fortraits$Dispersal), pch=unique(as.numeric(fortraits$Dispersal)), col=1)
savePlot("seedsize_vs_abun.emf", type="emf")
cor.test(log(fortraits$Total),fortraits$seed_size_mm) #rho=0.48, P=0.014 but spearman is ns
mean(rep(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], fortraits$top1cm3[!is.na(fortraits$seed_size_mm)]))
mean(rep(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], fortraits$base4cm3[!is.na(fortraits$seed_size_mm)]))
sd(rep(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], fortraits$top1cm3[!is.na(fortraits$seed_size_mm)]))
sd(rep(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], fortraits$base4cm3[!is.na(fortraits$seed_size_mm)]))
t.test((rep(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], fortraits$top1cm3[!is.na(fortraits$seed_size_mm)])), (rep(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], fortraits$base4cm3[!is.na(fortraits$seed_size_mm)])))

sizeabun.lm = lm((log(fortraits$Total)+1) ~ seed_size_mm, data=fortraits) 
sizeabun.res = resid(sizeabun.lm)
plot(fortraits$seed_size_mm[!is.na(fortraits$seed_size_mm)], sizeabun.res, col=as.factor(fortraits$Dispersal), ps=fortraits$seed_size_mm)
legend("topleft", legend=unique(fortraits$Dispersal), col=unique(fortraits$Dispersal)) 
abline(0, 0)
