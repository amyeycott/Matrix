source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
ls()
keep3<-!is.na(ferns[,1])&rowSums(ferns)>0&!is.na(env[,1])
keep3
ccafernfull<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+pH+N.+C.+TCat+CanO+m.from.stream,env[keep3,])
ccafernfull
anova(ccafernfull)
plot(ccafernfull)

ccafernempty<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~1, data=env[keep3,])
ordistep(ccafernempty, scope=formula(ccafernfull), perm.max = 1000)
## straight ordistep reports m from stream, slope, pH:3.6467 Constrained    0.5797

## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
ccafernsoil<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~pH+N.+C.+TCat,env[keep3,])
ordistep(ccafernempty, scope=formula(ccafernsoil), perm.max = 1000)
ccafernjustpH<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~pH,env[keep3,])
anova(ccafernjustpH)
#now the structure variables
ccafernstruc<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep3,])
ordistep(ccafernempty, scope=formula(ccafernstruc), perm.max = 1000)
#  returns empty model Constrained   0.12314    0.03377 
#now the topo variables
ccaferntopo<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~Slope+m.from.stream,env[keep3,])
ordistep(ccafernempty, scope=formula(ccaferntopo), perm.max = 1000)
ccafernjustslope<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~Slope,env[keep3,])
anova(ccafernjustslope)
#keeps slope (depending on what day it is, this and m.from.stream are very confounded). Constrained   
#now just forest
ccafernjustforest<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage,env[keep3,])
anova(ccafernjustforest)
ccafernjustforest
# no need for ordistep here; constrained inertia is   0.32939    0.09032    **
#final model: first test whether it's full
ccafernmodel<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+pH+Slope,env[keep3,])
ccafernmodel #Constrained  
vif.cca(ccafernmodel) #highest is 1.32 (forest)


ccafernmodelpart<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~pH+Slope+Condition(newage),env[keep3,])
ccafernmodelpart
#Constrained   



p<-plot(ccafernmodel, type="n")
colour<-c("black","grey15","lightgrey")
with(env[keep3,], ordisurf(ccafernmodel, m.from.stream, main="Ferns", nlevels=3, col=1))
points(ccafernmodel, display="sites", col=colour[env$newage[keep3]], pch=16)
legend(locator(1),c("og","os","ys"),pch=c(16,16,16),col=colour)
savePlot("ferncca with ordisurf.emf")


######## finished to here
##########this is set up for aspect, pH and forest. But we need a better plan if we're keeping aspect, m.from.stream, ph, TCat and C! 
ccaferns.FpluspH<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+pH,env[keep3,])
ccaferns.FpluspH
# xxx; xxx%
ccaferns.FpartpH<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+Condition(pH),env[keep3,])
ccaferns.FpartpH
# Conditional xxx; xxx%. Constrained xxx; xxx%
ccaferns.pHpartF<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~pH+Condition(newage),env[keep3,])
ccaferns.pHpartF
# Conditional xxx; xxxx%. Constrained xxx; xxx%
ccaferns.FplusA<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+aspect,env[keep3,])
ccaferns.FplusA
#xxx; xxx%
ccaferns.FpartA<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+Condition(aspect),env[keep3,])
ccaferns.FpartA
# Conditional xxx; xxx%. Constrained xxx; xxx%
ccaferns.ApartF<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~aspect+Condition(newage),env[keep3,])
ccaferns.ApartF
# Conditional xxx; xxx%. Constrained xxx; xxx%
ccaferns.pHplusA<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~pH+aspect,env[keep3,])
ccaferns.pHplusA
# xxx; xxx%
ccaferns.pHpartA<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~pH+Condition(aspect),env[keep3,])
ccaferns.pHpartA
# Conditional xxx; xxx%. Constrained xxx; xxx%
ccaferns.ApartpH<- cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~aspect+Condition(pH),env[keep3,])
ccaferns.ApartpH
# Conditional xxx; xxx%. Constrained xxx; xxx%
#threeway model
ccafern.FplusApluspH<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+pH+aspect,env[keep3,])
ccafern.FplusApluspH
#xxxx; xxxx%


#full end model
ccafern.FplusApluspHplusM<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+pH+Aspect+m.from.stream,env[keep3,])

#xxxx; xxxx%



keep<-!is.na(env$newage)&!is.na(ferns[,1])
library(indicspecies)
fernpatt<-multipatt(ferns[keep,], env$newage[keep], func="r")
summary(fernpatt)
p.adjust(fernpatt$sign$p.value, method = "BH")

#making the bar plots of species habitat affiliation - remember to run keep3 from line 8 above!!!!
temp<-merge(ferns[keep3,],env$newage, by="row.names") 
#colnames(temp)<-rownames(fern_hab) # works but they are different order as well as lengths, by three (add rownmaes col, and names, but need to find out about Drynaria sp., Cal pro (Diplazium proliferum?), and Platycerium sp. (elaph?), and  Pteris preusii in fern habit file is actually cateroptera)
temp
str(temp)
fern_hab

fernhab2<-rbind(fern_hab, "platy elaph")
fernhab2[29,1]<-"Polypodiaceae"
fernhab2[29,2]<-"Fern"
fernhab2[29,3]<-"F"
fernhab2[29,0]<-"Platycerium sp./eleph."
rownames(fernhab2)[29]<-"Platycerium sp./eleph."  # is not working, nor is rownames. Doesn't give an error message, but doesn't work either...


fernsYS<-temp[temp$y=="ys",2:30]
fernsYS<-t(fernsYS)
fernsYS
YSstep1<-merge(fern_hab,fernsYS, by.x=0, by.y=0, all=FALSE)
str(YSstep1)
YSstep1$total<-rowSums(YSstep1[,5:18]>0) 
YSstep1
YSsums<-xtabs(total~Habitat, data=YSstep1)
YSsums

fernsOS<-temp[temp$y=="os",2:30]
fernsOS<-t(fernsOS)
fernsOS
OSstep1<-merge(fern_hab,fernsOS, by.x=0, by.y=0, all=FALSE)
dim(OSstep1)
OSstep1$total<-rowSums(OSstep1[,5:19]>0) 
OSstep1
OSsums<-xtabs(total~Habitat, data=OSstep1)
OSsums

fernsOG<-temp[temp$y=="og",2:30]
fernsOG<-t(fernsOG)
fernsOG
OGstep1<-merge(fern_hab,fernsOG, by.x=0, by.y=0, all=FALSE)
dim(OGstep1)
OGstep1$total<-rowSums(OGstep1[,5:12]>0) 
OGstep1
OGsums<-xtabs(total~Habitat, data=OGstep1)
OGsums

sums<-cbind(YSsums, OSsums, OGsums)
 sums


barplot(sums, ylim=c(0,160), xlab="Age class", ylab="Sum of occurrences of each affiliation group", legend=c("F", "FF", "G"), main="Ferns")
savePlot("ferns_affiliations.emf", type="emf")

amynewFunc <- function(transVec, comMat)
{
	sapply(X = as.character(unique(transVec)), FUN = function(curTrans, comMat, transVec)
	{
		selectVec <- curTrans == transVec
		specTotals <- apply(X = comMat[selectVec, ], FUN = mean, MARGIN = 2)
	}, comMat = comMat, transVec = transVec)
}


write.csv(round(amynewFunc(temp$y, temp[,2:30]),3),"ferns.csv")



#fernsb<-ferns

#fern data - old way
#fern data including empty transects
#ferns<-read.table("fernsppswithempties.txt", sep="\t", header=T, row.names=1)
#head(ferns)
#ferns<-ferns[order(rownames(ferns)),]
#fern abundance (min val of range given by jenny) for abundance figures
#fernabun<-read.table("foresttypeabundances.txt" , sep="\t", header=T, row.names=1)
#names(ferns)
#colnames(ferns)<-c("Asplenium macrophlebium","Asplenium unilaterale","Asplenium inequilaterale","Asplenium gemmiferum","Asplenium erectum","Asplenium holstii","Asplenium warneckei","Pteris hamulosa","Pteris atrovirens/burtonii","Pteris pteridioides","Pteris catoptera","Pteris dentata","Bolbitis auriculata","Bolbitis gemmifera","Bolbitis acrostichoides","Tectaria gemmifera","Tectaria angelicifolia","Christella parasitica","Doryopteris kirkii","Microsorum punctatum","Microlepia speluncae","Pellaea doniana","Arthopteris palisoti","Nephrolepis biserrata","Pneumatopteris unita","Lomariopsis warneckei","Drynaria sp", "CAL_PRO","Cyclosorus interruptus","Platycerium sp./eleph.")

#setdiff(colnames(ferns), colnames(fatmax))


#fernsb[is.na(fernsb)] <- 0 
#fernsb<-fernsb[,order(names(fernsb))]

#write.table(fernsb, file="fernsb temp.txt")
#write.table(fatmax, file="ferntest temp.txt")


# This returns the number of cell mismatches per column?
#sapply(colnames(fatmax), function(n)sum(fatmax[,n]!=fernsb[,n])) #33
#sapply(colnames(fatmedian), function(n)sum(fatmedian[,n]!=fernsb[,n]))#32 - difference is one Pteris catoptera. Is there any difference anyway, or is each transect assigned reps? Pteris catoptera looks like the only one where there's a difference - transect 51 has a four in among the threes.

#write.table((fernsb-fatmax), file="error temp.txt")

   #fatmax, assembled from the xtabs of the thin table, looks at first to be ten times that of ferns (jenny's original fat table). But the above test makes some match better with the original xtabs, and some with the 
      
    
    
    