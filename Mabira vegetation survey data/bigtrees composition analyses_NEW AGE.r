source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

keep5<-!is.na(trees[,1])&rowSums(trees)>0&!is.na(env[,1])
keep5
head(trees)

boxplot(rowSums(trees>0)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Large trees rich by newage age", xlab="newage Age", ylab="Spprich")
savePlot("boxplot large tree rich by forest age.emf")

trees



#big trees is counts not percentages so we do not necessarily need to transform the data. The DCA is to check axis length, which was plenty long enough!
dcabigtrees<-decorana(trees[keep5,colSums(trees>0, na.rm=T)>1])
dcabigtrees

ccabigtreesfull<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+pH+N.+C.+TCat+CanO+m.from.stream,env[keep5,])
ccabigtreesfull
anova(ccabigtreesfull)

plot(ccabigtreesfull)
points(ccabigtreesfull, display="sites", col=as.numeric(env$newage[keep5])+1, pch=16)
identify(ccabigtreesfull, what="sites")


ccabigtrees.null<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~1, data=env[keep5,])
plot(ccabigtrees.null)
points(ccabigtrees.null, display="sites", col=as.numeric(env$newage[keep5])+1, pch=16)
identify(ccabigtrees.null, what="sites")
savePlot("cca bigtrees null.emf")


ordistep(ccabigtrees.null, scope=formula(ccabigtreesfull), perm.max = 1000)
# ordistep chooses newage.


## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
ccabigtreesoil<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~pH+N.+C.+TCat,env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreesoil), perm.max = 1000)
ccabigtreejusttcat<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~TCat,env[keep5,])
anova(ccabigtreejusttcat)
# chooses TCat Constrained   0.34407    0.07243 
#now the structure variables
ccabigtreestruc<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreestruc), perm.max = 1000)
# chooses null model
#now the topo variables
ccabigtreestopo<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~Slope+m.from.stream,env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreestopo), perm.max = 1000)
#keeps  null model
#now just forest
ccabigtreesjustforest<-cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~newage,env[keep5,])
anova(ccabigtreesjustforest)
ccabigtreesjustforest
# no need for ordistep here; anova says model is sig P=0.005, Constrained    0.5690     0.1198  
# then all the combinations of forest and TCat (from help file: the following commands are equivalent: cca(X, Y, Z), cca(X ~ Y + Condition(Z)), where Y and Z refer to constraints and conditions matrices respectively. )
ccabigtrees.FplusTCat<- cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~newage+TCat,env[keep5,])
ccabigtrees.FplusTCat
# Constrained    0.7539     0.1587 
ccabigtrees.FpartTCat<- cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~newage+Condition(TCat),env[keep5,])
ccabigtrees.FpartTCat
#   Constrained   0.40978    0.08626 
ccabigtrees.TCatpartF<- cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~TCat+Condition(newage),env[keep5,])
ccabigtrees.TCatpartF
#  Constrained   0.18485    0.03891
vif.cca(ccabigtrees.FplusTCat)
anova(ccabigtrees.FplusTCat)


# F3,25=1.572, P=0.013
p<-plot(ccabigtrees.FplusTCat, type="n")
colour<-c("black","grey15","lightgrey")
with(env[keep5,], ordisurf(ccabigtrees.FplusTCat, TCat, main="Large trees", col=1))
points(ccabigtrees.FplusTCat, display="sites", col=colour[env$newage[keep5]], pch=16)
#identify(p, what="sites")
#legend(locator(1),c("ys","os","og"),pch=c(16,16,16),col=sort(colour, decreasing=T))
savePlot("cca bigtrees tcat ordisurf.emf")

 
keep5b<-!is.na(env$newage)&!is.na(trees[,1])
library(indicspecies)
treepatt<-multipatt(trees[keep5b,], env$newage[keep5b], func="r")
summary(treepatt)
p.adjust(treepatt$sign$p.value, method = "BH")




#remember to run keep5 from line 3 above!!!!
temp<-merge(trees[keep5,],env$newage, by="row.names") 
temp
str(temp)

bigtreesYS<-temp[temp$y=="ys",2:51]
bigtreesYS<-t(bigtreesYS)
bigtreesYS
YSstep1<-merge(ang_hab,bigtreesYS, by.x="Species_Name", by.y="row.names", all=FALSE)
str(YSstep1)
YSstep1$total<-rowSums(YSstep1[,5:15]>0) 
YSstep1
YSsums<-xtabs(total~Habitat, data=YSstep1)
YSsums

bigtreesOS<-temp[temp$y=="os",2:51]
bigtreesOS<-t(bigtreesOS)
bigtreesOS
OSstep1<-merge(ang_hab,bigtreesOS, by.x="Species_Name", by.y="row.names", all=FALSE)
dim(OSstep1)
OSstep1$total<-rowSums(OSstep1[,5:18]>0) 
OSstep1
OSsums<-xtabs(total~Habitat, data=OSstep1)
OSsums

bigtreesOG<-temp[temp$y=="og",2:51]
bigtreesOG<-t(bigtreesOG)
bigtreesOG
OGstep1<-merge(ang_hab,bigtreesOG, by.x="Species_Name", by.y="row.names", all=FALSE)
dim(OGstep1)
OGstep1$total<-rowSums(OGstep1[,5:8]>0) 
OGstep1
OGsums<-xtabs(total~Habitat, data=OGstep1)
OGsums

sums<-cbind(YSsums, OSsums, OGsums)
 sums


barplot(sums, ylim=c(0,160), xlab="Age class", ylab="Sum of occurrences of each affiliation group", legend=c("F", "FF", "G"), main="Trees >10cm dbh")
savePlot("bigtrees_affiliations.emf")

amynewFunc <- function(transVec, comMat)
{
	sapply(X = as.character(unique(transVec)), FUN = function(curTrans, comMat, transVec)
	{
		selectVec <- curTrans == transVec
		specTotals <- apply(X = comMat[selectVec, ], FUN = mean, MARGIN = 2)
	}, comMat = comMat, transVec = transVec)
}


write.csv(round(amynewFunc(temp$y, temp[,2:51]),3),"bigtrees.csv")


#messing about with collins' successional stage, taken from Lwanga 2003
setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\KampalaFragments\\")
library(ODB)
con<-odb.open("FS1database.odb")
traits<-odb.read(con,'SELECT*from "Taxa"')
odb.close(con)

setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\")

traits$sppname<-paste(traits$Genus, traits$Species, sep=" ")
dim(traits)
trytreesOG<-merge(OGstep1, traits, by.x=1, by.y=24, all.x=T)
trytreesOG<-trytreesOG[,c(1,4,9,32)]

trytreesOS<-merge(OSstep1, traits, by.x=1, by.y=24, all.x=T)
trytreesOS<-trytreesOS[,c(1,4,9,42)]

trytreesYS<-merge(YSstep1, traits, by.x=1, by.y=24, all.x=T)
trytreesYS<-trytreesYS[,c(14,9,32)]