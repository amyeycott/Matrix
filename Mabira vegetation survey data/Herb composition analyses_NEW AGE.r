source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

# 'keep2' is a grid of true/false values that selects those columns of 'trueherbs' that have at least one ocurrence in them (!is.na means 'is not na'), and those transects which have some data for trueherbs and also for env.
keep2<-!is.na(trueherbs[,1])&rowSums(trueherbs)>0&!is.na(env[,1])
keep2

#run a decorana and look at the axis length of axis 1. If it is less than 1.5, consider rda. If it is more than 2, consider ca/cca/dca.   Here it is 3.1394
trueherbsdca<-decorana(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])
trueherbsdca


# first we run a cca with all the environment data in it, just to have a look. 
ccatrueherbfull<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~newage+Slope+Aspect+BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+pH+N.+C.+TCat+CanO+m.from.stream,env[keep2,])
ccatrueherbfull                            
anova(ccatrueherbfull)
# next we want to run ordistep: backward selection model, to get an idea which variables might have an influence. Ordistep needs a 'null' model with no environment data in (ie a ca) to compare the models it buils to. So we have to make that first. But we have to set it up in the same way as for a cca with envirnomental stuff in it. Ordistep returns the best model it can find to explain the data: the model it likes is in the output in the line that statrts 'Call:'
ccatrueherbs.null<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~1, data=env[keep2,])
ordistep(ccatrueherbs.null, scope=formula(ccatrueherbfull), perm.max = 1000)
# ordistep keeps newage and LLcov 

## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
# just forest
ccatrueherbjustforest<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~newage,env[keep2,])
anova(ccatrueherbjustforest)
ccatrueherbjustforest
# no need for ordistep here; anova says model is ns, Constrained   0.22051    0.09535
#now the structure variables
ccatrueherbstruc<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep2,])
ordistep(ccatrueherbs.null, scope=formula(ccatrueherbstruc), perm.max = 1000)
ccatrueherbllcov<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~LLCover,env[keep2,])
anova(ccatrueherbllcov)
#chooses  LLCover, Constrained   0.16129    0.06974
#soils
ccatrueherbsoil<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~pH+N.+C.+TCat,env[keep2,])
ordistep(ccatrueherbs.null, scope=formula(ccatrueherbsoil), perm.max = 1000)
# chooses null model
#now the topo variables
ccatrueherbtopo<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~Slope+Aspect+m.from.stream,env[keep2,])
ordistep(ccatrueherbs.null, scope=formula(ccatrueherbtopo), perm.max = 1000)
#chooses null model

# then all the combinations of forest and significant variables (from help file: the following commands are equivalent: cca(X, Y, Z), cca(X ~ Y + Condition(Z)), where Y and Z refer to constraints and conditions matrices respectively. )
ccatrueherbs.FplusL<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~newage+LLCover,env[keep2,])
ccatrueherbs.FplusL
vif.cca(ccatrueherbs.FplusL)
#  Constrained    0.3792     0.1639
ccatrueherbs.FpartL<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~newage+Condition(LLCover),env[keep2,])
ccatrueherbs.FpartL
# Constrained   0.21787    0.09421 
ccatrueherbs.LpartF<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~LLCover+Condition(newage),env[keep2,])
ccatrueherbs.LpartF
#  Constrained   0.15865    0.06860 

anova(ccatrueherbs.FplusL)
# F3,27=1.765, P=0.025
colour<-c("black","grey15","lightgrey")
p<-plot(ccatrueherbs.FplusL, type="n")
with(env[keep2,], ordisurf(ccatrueherbs.FplusL, LLCover, main="Herbs", col=1, nlevels=6))
points(ccatrueherbs.FplusL, display="sites", col=colour[env$newage[keep2]], pch=16)
legend(locator(1),c("og","os","ys"),pch=c(16,16,16),col=colour)
savePlot("cca herbs LLcov ordisurf.emf")




keep<-!is.na(env$newage)&!is.na(trueherbs[,1])
#correlation (point-biserial, a generalisation of phi for abundance data) instead of indval
library(indicspecies)

herbpatt<-multipatt(trueherbs[keep,], env$newage[keep], func="r")
summary(herbpatt)
p.adjust(herbpatt$sign$p.value, method = "BH")

#habit by ageclass
str(trueherbs)#31spp, 74sites inc na
str(ang_hab) 
env

#remember to run keep2 from line 4 above!!!!
temp<-merge(trueherbs[keep2,],env$newage, by="row.names") 
str(temp)

trueherbYS<-temp[temp$y=="ys",2:32] #takes out rownames and totals left over from the merge function
trueherbYS<-t(trueherbYS)
trueherbYS
YSstep1<-merge(ang_hab,trueherbYS, by.x="Species_Name", by.y="row.names", all=FALSE) #attaches habitat to species
YSstep1$total<-rowSums(YSstep1[,5:18]>0) 
YSstep1
YSsums<-xtabs(total~Habitat, data=YSstep1)
YSsums

trueherbOS<-temp[temp$y=="ms",2:32]
trueherbOS<-t(trueherbOS)
trueherbOS
OSstep1<-merge(ang_hab,trueherbOS, by.x="Species_Name", by.y="row.names", all=FALSE)
dim(OSstep1)
OSstep1$total<-rowSums(OSstep1[,5:13]>0) 
OSstep1
OSsums<-xtabs(total~Habitat, data=OSstep1)
OSsums

trueherbOG<-temp[temp$y=="og",2:32]
trueherbOG<-t(trueherbOG)
trueherbOG
OGstep1<-merge(ang_hab,trueherbOG, by.x="Species_Name", by.y="row.names", all=FALSE)
dim(OGstep1)
OGstep1$total<-rowSums(OGstep1[,5:8]>0) 
OGstep1$mean<-rowSums(OGstep1[,5:8])/4
OGstep1
OGstep1<-OGstep1[OGstep1$total>0,]
OGsums<-xtabs(total~Habitat, data=OGstep1)
OGsums

sums<-cbind(YSsums, OSsums, OGsums)
 sums
 
barplot(sums, ylim=c(0,150), xlab="Age class", ylab="Sum of occurrences of each affiliation group", legend=c("F", "FF", "G"), main="Herbs")
savePlot("Herb_affiliations.emf", type="emf")

#for the esm on composition
amynewFunc <- function(transVec, comMat)
{
	sapply(X = as.character(unique(transVec)), FUN = function(curTrans, comMat, transVec)
	{
		selectVec <- curTrans == transVec
		specTotals <- apply(X = comMat[selectVec, ], FUN = mean, MARGIN = 2)
	}, comMat = comMat, transVec = transVec)
}


write.csv(round(amynewFunc(temp$y, temp[,2:32]),3),"herbs.csv")