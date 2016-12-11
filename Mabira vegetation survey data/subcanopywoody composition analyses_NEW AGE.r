source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

### this file uses both the trees below 10cm dbh, and the shrubs.
keep6<-!is.na(subcanopywoody[,1])&rowSums(subcanopywoody)>0&!is.na(env[,1])
keep6
dcasubcanopywoodyfull<-decorana(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1]))
dcasubcanopywoodyfull
plot(dcasubcanopywoodyfull)

#note that shrub cover is removed, unlike for the herbs and ferns, because the shrubs are in the ordination.
ccasubcanopywoodyfull<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+pH+N.+C.+TCat+CanO+m.from.stream,env[keep6,])
ccasubcanopywoodyfull
anova(ccasubcanopywoodyfull)

plot(ccasubcanopywoodyfull, type="n")
points(ccasubcanopywoodyfull, display="sites", col=as.numeric(env$newage[keep6])+1, pch=16)

ccawithnothingin<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~1, data=env[keep6,])
p<-plot(ccawithnothingin, type="n")
points(ccawithnothingin, display="sites", col=as.numeric(env$newage[keep6])+1, pch=16)
points(ccawithnothingin, display="species", col=1, pch=1)
identify(p, what="sites")
identify(p, what="species")
savePlot("shrub cca with nothing in.emf")
ordistep(ccawithnothingin, scope=formula(ccasubcanopywoodyfull), perm.max = 1000)
# ordistep keeps Forest newage


## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
ccasubcanopywoodysoil<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~pH+N.+C.+TCat,env[keep6,])
ordistep(ccawithnothingin, scope=formula(ccasubcanopywoodysoil), perm.max = 1000)
 ccasubcanopywoodyjusttcat<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~TCat,env[keep6,])
anova(ccasubcanopywoodyjusttcat)
# chooses  TCat  Constrained   0.163093    0.05248
#now the structure variables - note shrub cover is removed!
ccasubcanopywoodystruc<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~BA+Deadwood+Stumps+LLCover+LLDepth+CanO,env[keep6,])
ordistep(ccawithnothingin, scope=formula(ccasubcanopywoodystruc), perm.max = 1000)
ccasubcanopywoodylldep<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~+LLDepth,env[keep6,])
anova(ccasubcanopywoodylldep)
#now the topo variables
ccasubcanopywoodytopo<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~Slope+m.from.stream,env[keep6,])
ordistep(ccawithnothingin, scope=formula(ccasubcanopywoodytopo), perm.max = 1000)
# chooses null model 
#now just forest
ccasubcanopywoodyjustforest<-cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~newage,env[keep6,])
anova(ccasubcanopywoodyjustforest)
ccasubcanopywoodyjustforest
# no need for ordistep here; anova says is significant, Constrained   0.29789    0.09614 
# then all the combinations of forest and TCat (from help file: the following commands are equivalent: cca(X, Y, Z), cca(X ~ Y + Condition(Z)), where Y and Z refer to constraints and conditions matrices respectively. )
ccasubcanopywoody.FplusTCatplusLLDepth<- cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~newage+TCat+LLDepth,env[keep6,])
ccasubcanopywoody.FplusTCatplusLLDepth
vif.cca(ccasubcanopywoody.FplusTCatplusLLDepth)
# Constrained    
#ccasubcanopywoody.FpartTCat<- cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~newage+Condition(TCat),env[keep6,])
ccasubcanopywoody.FpartTCat
# Constrained  
ccasubcanopywoody.TCatplusLLDepthpartF<- cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~TCat+LLDepth+Condition(newage),env[keep6,])
ccasubcanopywoody.TCatplusLLDepthpartF
# Constrained   

colour<-c("black","grey15","lightgrey")
anova(ccasubcanopywoody.FplusTCatplusLLDepth)
# F4,26=1.256, P 0 0.005
plot(ccasubcanopywoody.FplusTCatplusLLDepth, type="n")
with(env[keep6,], ordisurf(ccasubcanopywoody.FplusTCatplusLLDepth, TCat, nlevels=5, main="Sub-canopy woody plants", col=1, labcex=1.2))
points(ccasubcanopywoody.FplusTCatplusLLDepth, display="sites", col=colour[env$newage[keep6]], pch=16)

legend(locator(1),c("og","os","ys"),pch=c(16,16,16),col=colour)
savePlot("treeseedling cca with ordisurf.emf")


# INDICSPP - tells us which species are indicative of a particular forest type, Seems more stable than indval

keep5b<-!is.na(env$newage)&!is.na(trees[,1])

keep6b<-!is.na(subcanopywoody[,1])&!is.na(env$newage)
library(indicspecies)
scwpatt<-multipatt(subcanopywoody[keep6b,], env$newage[keep6b], func="r")
summary(scwpatt)
p.adjust(scwpatt$sign$p.value)

oldcomps<-c(192,202,226)

str(thinspp)






#remember to run keep6 from line 4 above!!!!
temp<-merge(subcanopywoody[keep6,],env$newage, by="row.names") 
temp
str(temp)

x11(); par(mfrow=c(2,2))
boxplot(temp[,78]~temp$y, main="Ouratea densiflora", ylab="percent cover")
barplot(c(sum(temp[temp$y=="og",78]>0), sum(temp[temp$y=="os",78]>0), sum(temp[temp$y=="ys",78]>0)), ylab="frequency", xlab="stand age", main="sig for og with r=0.672**")
boxplot(temp[,70]~temp$y, main="Acalypha neptunica", ylab="percent cover")
barplot(c(sum(temp[temp$y=="og",70]>0), sum(temp[temp$y=="os",70]>0), sum(temp[temp$y=="ys",70]>0)), ylab="frequency", xlab="stand age", main="sig for ys with r=0.452*")

 

subcanopywoodyYS<-temp[temp$y=="ys",2:83]
subcanopywoodyYS<-t(subcanopywoodyYS)
subcanopywoodyYS
YSstep1<-merge(ang_hab,subcanopywoodyYS, by.x="Species_Name", by.y="row.names", all=FALSE)
str(YSstep1)
YSstep1$total<-rowSums(YSstep1[,5:18]>0) 
YSstep1
YSsums<-xtabs(total~Habitat, data=YSstep1)
YSsums

subcanopywoodyOS<-temp[temp$y=="os",2:83]
subcanopywoodyOS<-t(subcanopywoodyOS)
subcanopywoodyOS
OSstep1<-merge(ang_hab,subcanopywoodyOS, by.x="Species_Name", by.y="row.names", all=FALSE)
dim(OSstep1)
OSstep1$total<-rowSums(OSstep1[,5:13]>0) 
OSstep1
OSsums<-xtabs(total~Habitat, data=OSstep1)
OSsums

subcanopywoodyOG<-temp[temp$y=="og",2:83]
subcanopywoodyOG<-t(subcanopywoodyOG)
subcanopywoodyOG
OGstep1<-merge(ang_hab,subcanopywoodyOG, by.x="Species_Name", by.y="row.names", all=FALSE)
dim(OGstep1)
OGstep1$total<-rowSums(OGstep1[,5:12]>0) 
OGstep1
OGsums<-xtabs(total~Habitat, data=OGstep1)
OGsums

sums<-cbind(YSsums, OSsums, OGsums)
 sums


barplot(sums, ylim=c(0,160), xlab="Age class", ylab="Sum of occurrences of each affiliation group", legend=c("F", "FF", "G"), main="Shrubs and saplings <10cm dbh")
savePlot("Subcanopywoody_affiliations.emf", type="emf")

amynewFunc <- function(transVec, comMat)
{
	sapply(X = as.character(unique(transVec)), FUN = function(curTrans, comMat, transVec)
	{
		selectVec <- curTrans == transVec
		specTotals <- apply(X = comMat[selectVec, ], FUN = mean, MARGIN = 2)
	}, comMat = comMat, transVec = transVec)
}


write.csv(round(amynewFunc(temp$y, temp[,2:83]),3),"scw.csv")