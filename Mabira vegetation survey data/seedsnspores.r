#index. Data loading & cleaning 2 - 42


source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")


#Just like in the above-ground data, there are lots of things that are classified as 'sp'. There are three possible routes. Turning them into the same species as found in that transect or the only congeneric in the whole dataset would work for Ficus, Solanum and Triumfetta, but cannot be done for Cyperus or Kyllinga. Merging like that also increases similarity where it might not be. Removing them removes 5 ocurrences, there are also 5 unidentified spp to remove. This is out of 72 ocurrences recorded, ie 12% of the data (6% unidentified, 6% id to genus). Leaving them in increases diversity artificially and definitely looks wrong for ficus, though not the others.  Leaving them out could have either effect on similarity (increase or decrease). Here follows script done by leaving them out.
     
#this is the stuff in controls. All the things from seeds that occur in controls are recorded at a similar  or higher density in controls as samples, and npot in the (much bigger) community dataset so they can also all be removed!
                   
con2<-odbcConnectExcel("Seedbank Datasheet_corrected.xls2.xls")
sqlTables(con2)
contamination_thin<-sqlQuery(con2, "Select * From [Control trays$]")
contamination_thin<-contamination_thin[1:8,]
contamination_thin
close(con2) 
#density of contamination species things in the regular trays
dodgy<-seed.all[,names(seed.all) %in% contamination_thin$Species]
dodgydens<-colSums(dodgy, na.rm=T)/31590
dodgydens
dodgydens<-t(dodgydens)
dodgydens<-t(dodgydens)
unclass(dodgydens)
colnames(dodgydens)<-"in_samples"
dodgydens<-merge(dodgydens, contamination_thin, by.x=0, by.y=1,all.x=T, all.y=F) 
dodgydens$remaining<-(dodgydens$in_samples)-(dodgydens$per_cm3)
dodgydens
#same again for chr para. This is hard because of the space in the column name confusing r
contamination_thin
spore.all
dodgyspore<-(sum(spore.all[,2], na.rm=T))/31590
dodgyspore

#but it's not so simple for the spore bank because the one suspected contaminant is Cristella parasitica.  4 clusters were found in the controls (0.0004cm-3), which would account for 2% of the seedlings in the trays (0.018cm-3), . Therefore Chr para contamination is not addressed in this script

drops <- c("Unidentified1","Unidentified2","Unidentified3","Unidentified4","Unidentified5", "Cyperus sp", "Ficus sp","Kyallinga sp","Solanum sp", "Triumfetta sp", "Ageratum conyzoides","Conyza floribunda", "Oxalis corniculata", "Stachytarpheta urticifolia") 

seed.all<-seed.all[,!(names(seed.all) %in% drops)] 

#pull contamination out of thinseeds, then remove the unused levels of species 
thinseedsclean<-thinseeds[!thinseeds$Species%in%drops,]
thinseedsclean$Species<-factor(thinseedsclean$Species)
str(thinseeds)
str(thinseedsclean)

########Data manipulation and analysis begins here############### (switch to file seedsnspores_trees separate.r here, when doing separate analyses)     
# count by sample and depth. This remakes seed.layers (already made in 'matrix veg load data') without contamination
seed.layers<-aggregate(thinseedsclean[,6:7], by=list(thinseedsclean$Transect), FUN=sum, na.rm=TRUE)
seed.layers
t.test(seed.layers$top1cm3, seed.layers$base4cm3, paired=T)
#t = -2.6726, df = 38, p-value = 0.01103
#total area sampled - 39 transects, 10 samples, each of 2.6cm diameter. Each core is 5.31 cm2 and 26.55cm3. Each transect is 53.1cm2, total area is 2070.9cm2. Per transect volume is Top 1cm 53.1cm3, bottom  212.4cm3. total vol of all top layers is 2070.9cm3, bottom 4cm makes 8283.6cm3, total volume sampled 10354.5cm3 (about ten litres). 
seed.layers$denstop<-seed.layers$top1cm3/53.1
seed.layers$densbase<-seed.layers$base4cm3/212.4
seed.layers$perm2top<-(seed.layers$top1cm3/53.1)*(100*100)
seed.layers$perm2base<-(seed.layers$base4cm3/53.1)*(100*100)
seed.layers$totaldens<-seed.layers$denstop+seed.layers$densbase
seed.layers$perm2<-herbseed.layers$perm2top+herbseed.layers$perm2base
#t.test(seed.layers$denstop, seed.layers$densbase, paired=T, var.equal=F)#t = 3.4784, df = 38, p-value = 0.001281. Default is that variance is unequal, but that doesn't help the skew issue

median(seed.layers$percm2)

str(seed.layers)
wilcox.test(seed.layers$denstop, seed.layers$densbase, paired=T)

spore.layers<- aggregate(thinspores[,6:7], by=list(thinspores$Transect), FUN=sum, na.rm=TRUE)
spore.layers$denstop<-spore.layers$top1cm3/53.1
spore.layers$densbase<-spore.layers$base4cm3/212.4
spore.layers$perm2top<-(spore.layers$top1cm3/53.1)*(100*100)
spore.layers$perm2base<-(spore.layers$base4cm3/53.1)*(100*100)
spore.layers$perm2<-spore.layers$percm2*(100*100)
spore.layers
all.layers.dens<-merge(seed.layers, spore.layers, by=1)

wilcox.test(spore.layers$denstop, spore.layers$densbase, paired=T)

forbarplot<-cbind(all.layers.dens$denstop.x, all.layers.dens$densbase.x, all.layers.dens$denstop.y, all.layers.dens$densbase.y)
forbarplot<-as.data.frame(unclass(forbarplot))
colnames(forbarplot)<-c("seeds 0-1cm", "seeds 1-5cm", "spores 0-1cm", "spores 1-5cm")

boxplot(forbarplot, names=c("seeds 0-1cm", "seeds 1-5cm", "spores 0-1cm", "spores 1-5cm"), ylab="Density cm-3", cex.axis=0.75 )
savePlot("seed and spore density by layer.emf")

shapiro.test(forbarplot[,1])#W = 0.8808, p-value = 0.0007643
shapiro.test(forbarplot[,2]) #W = 0.8079, p-value = 1.523e-05
shapiro.test(forbarplot[,3]) #W = 0.3825, p-value = 1.796e-11
shapiro.test(forbarplot[,4])  #  W = 0.823, p-value = 3.193e-05
#all not-normal - leptokutic skew, and lots of zeros for spores

#if the paired sample t-test compares the differences to zero, then to do an equivalent to levene we could do a shapiro on the differences?
shapiro.test((forbarplot[,1]-forbarplot[,2])) #W = 0.9491, p-value = 0.08345
shapiro.test((forbarplot[,3]-forbarplot[,4])) #W = 0.7569, p-value = 1.55e-06
shapiro.test((forbarplot[,1]-forbarplot[,3])) #W = 0.5711, p-value = 2.375e-09
shapiro.test((forbarplot[,2]-forbarplot[,4]))  #W = 0.9026, p-value = 0.003026
#it's a matrix so apply works (not eg sapply). The 2 tells it to go column by column, if you want row by row use 1.
apply(forbarplot, 2,function(x){shapiro.test(x)})
apply(forbarplot, 2,function(x){median(x)})
apply(forbarplot, 2,function(x){quantile(x, probs=c(0.25,0.75), na.rm=TRUE, type=7)})
median(forbarplot[,1]+forbarplot[,2])
median(forbarplot[,3]+forbarplot[,4])
quantile((forbarplot[,1]+forbarplot[,2]), probs=c(0.25,0.75), na.rm = TRUE, type = 7)
quantile((forbarplot[,3]+forbarplot[,4]), probs=c(0.25,0.75), na.rm = TRUE, type = 7)


# makes a data frame with the species richnesses in
abunss<-sapply(list(spores=spore.all, seeds=seed.all), function(x){
  rowSums(x)
})
abunss<-as.data.frame(abunss)
abunss

# makes a data frame with the species richnesses in ##BROKEN, fix from split file because it uses a data file containing contamination#############
richss<-sapply(list(spores=spore.all, seeds=seed.all), function(x){
  rowSums(x>0)
})
richss<-as.data.frame(richss)
richss

byage<-merge(abunss, richss, by=0)
byage<-merge(byage, env$Forest, by.x=1, by.y=0)
rownames(byage)<-byage$Row.names


x11(point=16, height=6, width=6); par(mfrow=c(2,2), mar=c(2.5,2.5,2.5,0.5))
boxplot(byage$seeds.x~byage$y, main="Seed abundance *", cex.main=0.9, ylim=c(0,20))
boxplot(byage$spores.x~byage$y, main="Spore abundance ns", cex.main=0.9, ylim=c(0,60), yaxt="n")
axis(side=2, at=c(0,20,40,60))
boxplot(byage$seeds.y~byage$y, main="Seed spp richness ns", cex.main=0.9, ylim=c(0,5), yaxt="n")
axis(side=2, at=c(0,2,4))
boxplot(byage$spores.y~byage$y, main="Spore spp richness ns", cex.main=0.9, ylim=c(0,5), yaxt="n")
axis(side=2, at=c(0,2,4))
savePlot("seedsspores_by_age_pres.emf")

library(car)
leveneTest(byage$seeds.x, byage$y) 
leveneTest(byage$spores.x, byage$y)
leveneTest(byage$seeds.y, byage$y)
leveneTest(byage$spores.y, byage$y)
#all of these pass levene test
kruskal.test(byage$seeds.x, byage$y) 
kruskal.test(byage$spores.x, byage$y)
kruskal.test(byage$seeds.y, byage$y)
kruskal.test(byage$spores.y, byage$y)

x11(); par(mfrow=c(2,1))
plot (jitter(abunss$spores, amount = 0.1)~jitter(abunss$seeds, amount=0.01), col=as.factor(env$Forest), xlab="abun_seeds", ylab="abun_spores")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
plot (jitter(richss$spores, amount = 0.1)~jitter(richss$seeds, amount=0.01), col=as.factor(env$Forest), xlab="spprichseeds", ylab="spprichspores")
legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))
savePlot("pairs_seedsspores.emf")

cor.test(abunss$spores,abunss$seeds, method = "kendall")
cor.test(richss$spores, richss$seeds, method = "kendall")
cor.test(abunss$seeds, richss$seeds, method = "kendall")
cor.test(abunss$spores, richss$spores, method = "kendall")

x11();par(mfrow=c(2,2))
plot(abunss$spores,abunss$seeds)
plot(richss$spores, richss$seeds)
plot(abunss$seeds, richss$seeds)
plot(abunss$spores, richss$spores)

x11();par(mfrow=c(2,2))
mapply( function(x,n)hist(x, main= n),x=list(abunss$seeds, abunss$spores, richss$seeds, richss$spores),n=c("seed abundance", "spore abundance", "seed species richness","spore species richness"))
savePlot("seedspore_hists.pdf")

table(abunss$seeds)
table(abunss$spores)

sapply(list(abunss$seeds, abunss$spores, richss$seeds, richss$spores), function(x){
  sum(!is.na(x))
})

sapply(list(abunss$seeds, abunss$spores, richss$seeds, richss$spores), function(x){
  table((x))
})

which(rank(colSums(seed.all,na.rm=T), na.last = TRUE, ties.method = "average")>ncol(seed.all)-5, useNames = TRUE)
#returns 5 most abundant seed species but not their abundance (the number is their place on the list). Broussonetia papyrifera 125 Ficus sur and Pseudoantherum ludovicianum.
sort(colSums(seed.all,na.rm=T))
sort(colSums(seed.all>0,na.rm=T))
# last 5 entries are 5 most frequent seed species. Broussonetia papyrifera (32 samples), Piper capensis 9, Pseudoantherum ludovicianum 7, Ficus sur 7, Synedrella nodiflora 6.
sort(colSums(spore.all,na.rm=T))
#there are only 5 species. In order of abundance: Christella parasitica 578, Microlepia specuncea 19, Pallae viridis 8, Doryopteris concolor 1, Amauropelta oppositiformis 1
sort(colSums(spore.all>0,na.rm=T))
#Same order, 31 9 6 1 1 samples respectively

veg<-merge(trueherbs, subcanopywoody, by.x=0, by.y=0)
veg$Row.names<-as.factor(veg$Row.names)
rownames(veg)<-veg$Row.names
veg<-veg[2:131]
rowSums(veg, na.rm=T)
### there are some empty scw rows because of tree species with no saplings being included in the scw species lists
which(rank(colSums(veg,na.rm=T), na.last = TRUE, ties.method = "average")>ncol(veg)-5, useNames = TRUE)
which(rank(colSums(veg>0,na.rm=T), na.last = TRUE, ties.method = "average")>ncol(veg)-5, useNames = TRUE)
#for some reason these return the same results - it seems to be returning row numbers?.

sort(colSums(veg>0, na.rm=T)) #works properly
sort(colSums(veg, na.rm=T)) 
sort(colSums(ferns, na.rm=T))
sort(colSums(ferns>0, na.rm=T)) 

###species accumulation
keep8<-!is.na(herbs[,1]) #this matches veg so it doesn't matter
keep3c<-!is.na(ferns[,1])
keep7<-!is.na(seed.all[,1])
keep9<-!is.na(spore.all[,1])
spaccherb <- specaccum(herbs[keep8,], "exact")
spaccfern <- specaccum(ferns[keep3c,], "exact")
spaccseed <- specaccum(seed.all[keep7,], "exact")
spaccspore <- specaccum(spore.all[keep9,], "exact")                      
plot(spaccherb, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(1,70), xaxs="i", ylim=c(0,160), yaxs="i", bty="l", ylab="")
plot(spaccfern, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(spaccseed, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
plot(spaccspore, add=T, ci.type="line", col="gold", lwd=2, ci.lty=1, ci.col="palegoldenrod")
legend("topright",c("Herbs","Ferns","Seeds", "Spores"),pch=16,col=c("blue","green","red", "gold"))
savePlot("seedbank accumulation curves_bright colours.emf")       

###ordinations - we kept the nmds
library(vegan)
dca.subseta<-seed.all[!is.na(rowSums(seed.all)),]
#row 6 site 10 (OG) and row 17 site 36 (OS) are absolute outliers, ie a completely different composition to all the others. NMDS doesn't really cure this, so removing sites must be the way to go. 
dca.subsetb<-dca.subseta[!rownames(dca.subseta) %in% c(10,36),]
seed.dca<-decorana(dca.subsetb)
summary(seed.dca)
x11();par(mfrow=c(1,1))
plot(seed.dca)
seed.ca<-cca(dca.subsetb)
plot(seed.ca) #yuck. Stick with DCA which is a nice plot once the outliers are removed.
 
new.subset<-dca.subseta[!rownames(dca.subseta) %in% c(10,36),colSums(dca.subseta>0)>2]
seed.ca<-cca(new.subset)
plot(seed.ca)

seed.ca.age<-cca(new.subset~y, data=byage[!rownames(byage) %in% c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),])
plot(seed.ca.age, type="n")
points(seed.ca.age, disp="sites", col=byage[!rownames(byage) %in% c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),]$y)
anova(seed.ca.age)

nmds1<-metaMDS(dca.subsetb,k=2)
sco.nmds1<-scores(nmds1, choice=1:2, disp="sites")
spp.nmds1<-scores(nmds1, choice=1:2, disp="species")
plot(nmds1, type="n", cex=1.4) #is also pretty - this is the one in use
points(nmds1, display="sites", col=c(2,3,4)[byage[!rownames(byage) %in% c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),]$y], pch=16)
legend("bottomright", legend=levels(byage$y), col=c(2,3,4), pch=16, cex=1.4)
savePlot("seedbank NMDS_bright colours.emf") 

envfit(nmds1,byage[!rownames(byage) %in%  c(10,36),][!is.na(byage$seeds.x[!rownames(byage) %in% c(10,36)]),][,"y",drop=F], permutations=999, display="sites", na.rm=T)  

procrustes(nmds1, seed.dca, symmetric=T) #0.5287  so not so bad...

sco.dcaseeds<-scores(seed.dca, choices=1:2, disp="sites")
sco.dcaseeds<-merge(sco.dcaseeds, env[2], by.x=0, by.y=0)
sco.dcaseeds<-merge(sco.dcaseeds, sco.nmds1, by.x=1, by.y=0) 
leveneTest(sco.dcaseeds$DCA1, sco.dcaseeds$Forest)
leveneTest(sco.dcaseeds$DCA2, sco.dcaseeds$Forest)
leveneTest(sco.dcaseeds$NMDS1, sco.dcaseeds$Forest)
leveneTest(sco.dcaseeds$NMDS2, sco.dcaseeds$Forest)
#all of these pass levene test

#however you do the test, the following relationships are ns
kruskal.test(sco.dcaseeds$DCA1, sco.dcaseeds$Forest)
lm.DCA1<-lm(sco.dcaseeds$DCA1~sco.dcaseeds$Forest)
summary(lm.DCA1)
kruskal.test(sco.dcaseeds$DCA2, sco.dcaseeds$Forest)
lm.DCA2<-lm(sco.dcaseeds$DCA2~sco.dcaseeds$Forest)
summary(lm.DCA2)
kruskal.test(sco.dcaseeds$NMDS1, sco.dcaseeds$Forest)
lm.NMDS1<-lm(sco.dcaseeds$NMDS1~sco.dcaseeds$Forest)
summary(lm.NMDS1)
kruskal.test(sco.dcaseeds$NMDS2, sco.dcaseeds$Forest)
lm.NMDS2<-lm(sco.dcaseeds$NMDS2~sco.dcaseeds$Forest)
summary(lm.NMDS2)

######## similarity
seedsfordist1<-seed.all
rownames(seedsfordist1)<-as.factor(rownames(seedsfordist1))
seedsfordist1<-seedsfordist1[order(rownames(seedsfordist1)),]
seedsfordist1
vegfordist1<-veg
vegfordist<-vegfordist1[(!is.na(vegfordist1[,1])&!is.na(seedsfordist1[,1])),]
seedsfordist<-seedsfordist1[(!is.na(vegfordist1[,1])&!is.na(seedsfordist1[,1])),]

library(analogue)
allt<-join(seedsfordist, vegfordist, split=F)
allt<-cbind(seed.plant=factor(c(rep("seed", nrow(seedsfordist)) ,rep("plant",nrow(vegfordist)))), allt)

vegdistmat<-vegdist(allt[,2:153], method="bray", na.rm=T) 
vegdistmat<-as.matrix(vegdistmat)
distance.values<-c(vegdistmat[1,27],vegdistmat[2,28],vegdistmat[3,29],vegdistmat[4,30],vegdistmat[5,31],vegdistmat[6,32],vegdistmat[7,33],vegdistmat[8,34],vegdistmat[9,35],vegdistmat[10,36],vegdistmat[11,37],vegdistmat[12,38],vegdistmat[13,39],vegdistmat[14,40],vegdistmat[15,41],vegdistmat[16,42],vegdistmat[17,43],vegdistmat[18,44],vegdistmat[19,45],vegdistmat[20,46],vegdistmat[21,47],vegdistmat[22,48],vegdistmat[23,49],vegdistmat[24,50],vegdistmat[25,51],vegdistmat[26,52])
distance.transects<-c(1,15,17,20,21,24,26,27,3,36,38,39,4,45,47,48,57,59,6,60,62,66,68,71,72,74) 
distances<-cbind(distance.transects, distance.values)
distances2<-merge(distances,env, by.x=1, by.y=0)
kruskal.test(distances2$distance.values, distances2$Forest)
tapply(distances2$distance.values, distances2$Forest, FUN=median)

sort(names(all))

allsums<-by(allt[,2:153], allt$seed.plant, colSums, simplify=F)
allsums<-as.data.frame(cbind(plant=allsums$plant, seed=allsums$seed)) 
allsums<-allsums[(allsums$plant+allsums$seed)>0,] 
allsums<-allsums[order(allsums$plant, allsums$seed),]  #this likes a df
barplot(as.matrix(log(allsums+1)), beside=T, horiz=T, xlim=c(-1, 7), xlab="Log total abundance", col="white", yaxp=c(0, 40, 20), space=c(0,3)) #this likes a matrix
savePlot("Martaplot seeds vs veg.emf")     

### Flora Tropical East Africa classificat
str(seed.all)
str(thinseeds)
levels(thinseedsclean$Group)
Grasslist<-thinseedsclean[thinseedsclean$Group=="Grass",3]
Grasslist<-factor(Grasslist)
seedsgrasses<-seed.all[,which(names(seed.all) %in% Grasslist)]
Nonwoodylist<-thinseedsclean[thinseedsclean$Group=="Nonwoody",3]
Nonwoodylist<-factor(Nonwoodylist)
seedsNonwoody<-seed.all[,which(names(seed.all) %in% Nonwoodylist)]
Shrublist<-thinseedsclean[thinseedsclean$Group=="Shrub",3]
Shrublist<-factor(Shrublist)
seedsShrubs<-seed.all[,which(names(seed.all) %in% Shrublist)]
Treelist<-thinseedsclean[thinseedsclean$Group=="Tree",3]
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
sum(rowSums(sums_forhabit))
(sums_forhabit)/(rowSums(sums_forhabit))*100
colSums(sums_forhabit)/sum(colSums(sums_forhabit))*100
