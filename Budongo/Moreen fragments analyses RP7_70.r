source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Budongo\\moreen fragments load data.r")
#note to self: power analyses for correlation tests are easy. Do them at least for the env vars!
#also note to self: no pont doing HShann and diversity, they are RIDICULOUSLY correlated 0.977 P=0.0002

#turn the size of RP7 to 70
env$FragmentSize[19:21]<-70
envbyforest$FragmentSize[7]<-70
sums$FragmentSize[7]<-70
total$FragmentSize[total$Fragment.name=="RP7"]<-70


table(guildindex$Forest.specialisation,guildindex$Feeding.guild)


cor.test(sums$spprich,sums$FragmentSize)
cor.test(sums$spprich,sums$Distance)
cor.test(sums$rarefied,sums$FragmentSize)
cor.test(sums$rarefied,sums$Distance)
cor.test(sums$totalbirds,sums$FragmentSize)
cor.test(sums$totalbirds,sums$Distance)
cor.test(sums$FF,sums$FragmentSize)
cor.test(sums$FF,sums$Distance)  #sig and high neg r
cor.test(sums$spprich,sums$FragmentSize, method="spearman")
cor.test(sums$spprich,sums$Distance, method="spearman")
cor.test(sums$rarefied,sums$FragmentSize, method="spearman")
cor.test(sums$rarefied,sums$Distance, method="spearman")
cor.test(sums$totalbirds,sums$FragmentSize, method="spearman")
cor.test(sums$totalbirds,sums$Distance, method="spearman")
cor.test(sums$FF,sums$FragmentSize, method="spearman")
cor.test(sums$FF,sums$Distance, method="spearman")
cor.test((sums$FF/sums$totalbirds),sums$FragmentSize, method="spearman") #big neg rho -0,93**
cor.test((sums$FF/sums$totalbirds),sums$Distance, method="spearman")  #-0,89*
cor.test(sums$abun,sums$FragmentSize, method="spearman")
kruskal.test(sums$spprich, sums$Matrix.type)
kruskal.test(sums$abun, sums$Matrix.type)
kruskal.test(sums$rarefied, sums$Matrix.type)
kruskal.test(sums$FF, sums$Matrix.type)
kruskal.test((sums$FF/sums$totalbirds), sums$Matrix.type)

library(nlme)
lmerare<-lme(rarefied~Matrix.type+CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total) #ns, took outleast useful parameter (matrix)
lmerare.notmat<-lme(rarefied~CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total)  #worse significance for parameters, AIC is a little better
lmeFF<-lme(FF~Matrix.type+CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total) #ns but good balance between stdev of fixed vs random effects, took outleast useful parameter (matrix). plot is funnely
lmeFF.notmat<-lme(FF~CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total)# 
lmeFF.interactions<-lme(FF~Matrix.type+CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total)# too many degrees of freedom, doesn't run. Even just one interaction gives 'not a number' in some p values.
lmeGRIN<-lme(GRIN~Matrix.type+CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total) #ns but good balance between stdev of fixed vs random effects. plot is super-funnely
lmeGRIN.notmat<-lme(GRIN~CanopyOpeness+FragmentSize+Distance,random=~+1|Fragment.name, data=total)# horrible in all the ways
lmeGRIN.notdis<-lme(GRIN~Matrix.type+CanopyOpeness+FragmentSize,random=~+1|Fragment.name, data=total)# still pretty crap




##### ordination stuff begins here#####
str(spp)
dca<-decorana(spp)
dca #axis length 2.53. A bit triangly, site outlierness not too bad
plot(dca)
ca<-cca(spp)
plot(ca)#horseshoe yuck, actually mostly caused by an outlier, Rwesama2, but even without singletons that's very archy
ca_nosings<-cca(spp[,colSums(spp>0)>1])
plot(ca_nosings)
dca_nosings<-decorana(spp[,colSums(spp>0)>1])
plot(dca_nosings)#not a great improvement on the original dca, but then that has downweighting of rare species as default. 

dca
ca
dca_nosings
ca_nosings

ccafull<-cca(spp[,colSums(spp>0)>1]~., data=env[,c(2,5,6,7)])
#explains just under half the variation on axes 1 and 2 that an unconstrained ordination was able to. But the amount remaining on the first unconstrained axis (after the 8 or so constrained ones) is SIX times that of CCA1, which suggests it's pretty bad. Would an indirect one be better, and in that case, should we go for mds and envfit correlations?
ccafull
summary(ccafull)
plot(ccafull)
vif.cca(ccafull)  #none are bigger than 20 so it's probably an ok model (highest is distance at 13)
minmod<-step(ccafull, test="perm")#starting with the full model. Gives fragment size as the thing with the highest individual AIC - but I don't know how P values work because it gives 0.99
minmod2<-ordistep(ca_nosings, scope=formula(ccafull), perm.max = 1000)#not working at the moment, even though step (above)is

rowSums(spp)
spp
mds <- metaMDS(spp)
plot(mds, type="n")
points(mds)   #neat, but not that great
envfit(mds, env[,c(2,5,6,7)], permutations=999, display="sites", na.rm=T)
#suddenly works rather well.
envfit(mds, env$Matrix.type, permutations=999, display="sites", na.rm=T)
#even this has dreadful fit
mds


dcadaily<-decorana(daily)
dcadaily #axis length 3.83. A bit triangly, site outlierness not too bad
plot(dcadaily)
cadaily<-cca(daily)
plot(cadaily)# 3 bad outliers, but with no singletone is worse
mdsdaily <- metaMDS(daily)
plot(mdsdaily)
mdsdaily#stress 0.244
scores<-as.data.frame(scores(mdsdaily))

bu<-(max(scores$NMDS1[1:5]))-(min(scores$NMDS1[1:5]))
ka<-(max(scores$NMDS1[6:10]))-(min(scores$NMDS1[6:10]))
ki<-(max(scores$NMDS1[11:15]))-(min(scores$NMDS1[11:15]))
on<-(max(scores$NMDS1[16:20]))-(min(scores$NMDS1[16:20]))
RP7<-(max(scores$NMDS1[21:25]))-(min(scores$NMDS1[21:25]))
rw<-(max(scores$NMDS1[26:30]))-(min(scores$NMDS1[26:30]))
te<-(max(scores$NMDS1[31:35]))-(min(scores$NMDS1[31:35]))
NMDS1range<-as.data.frame(rbind(bu,te,on,rw,ka,ki,RP7))
rownames(NMDS1range)<-row.names(sums)
names(NMDS1range)<-"NMDS1range"
sums<-merge(sums,NMDS1range, by=0)


#use betadiv instead

boxplot(NMDS1range~Group.1, data=sums, col=c("gray27","gray27","gray57","gray57","gray87","gray87","gray7"), ylab="Range in NMDS1 scores", las=2)
points(sums$Group.1,sums$special,col="black", pch=16, cex=(sqrt(sums$FragmentSize)/4))
legend("bottomright",c("Bushland","Farmland","Sugarcane", "Continuous Forest"),pch=15,col=c("gray27","gray57","gray87","gray7"))
savePlot("rarefied spprich per trasect and forest.emf") 

out <- nestedtemp(spp) #31.95
plot(out)
out
plot(out, kind="incid")
oecosimu(spp, nestedchecker, "quasiswap", nsimul=999) # C score5.34, units 14859, p (sim) 0.63
library(bipartite)
#nestedness(spp, null.models = TRUE, n.nulls = 100, popsize = 30, n.ind = 7, n.gen = 2000, binmatnestout=FALSE) ##Do not run, crashy crashy 


#rank abundance
abun<-xtabs(No..of.Birds~Botanical.name, data=sppthin)
abun<-as.data.frame(unclass(abun))
names(abun) <-'abundance'
abun<-sort(abun$abundance)
abun<-as.data.frame(unclass(abun))
names(abun) <-'abundance'
abun

kind<-sppthin[,c(8,11)]
kind<-unique(kind)
head(kind)
head(abun)
abun<-merge(abun,kind, by.x=0, by.y=2, sort=FALSE)
# not necessary: abun$rank<-rank(abun$abundance)

barplot(log(abun$abundance)+1, names.arg="", xlab="Species ordered by abundance", ylab="log Abundance", col=abun$Forest.specialisation)
legend(locator(1),c("FF","F","FV","NF", "O"),pch=15,col=c("red","black","green","blue", "cyan"))
savePlot("rank abun by habspec.emf")


distmat<-vegdist(spp, method="bray", na.rm=T) 
betadisper<-betadisper(distmat, total$Fragment.name) 
anova(betadisper) #ns
distmat<-as.matrix(distmat)
distmat


#power analysis

power.t.test(n = NULL, delta=(19.17-13.33),sd=4.26, sig.level=0.05, power=0.8, type="two.sample")#just for interest, this is testing how to get a difference in spprich using the means for farm (largest of all means) and sugarcane(smallest of all means) using the larger of the two sd's. It says n=9.41, ie we'd need two more forests (might have come close with one more forest, n=9)




