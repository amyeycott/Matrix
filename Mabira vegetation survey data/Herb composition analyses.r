source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

# 'keep2' is a grid of true/false values that selects those columns of 'trueherbs' that have at least one ocurrence in them (!is.na means 'is not na'), and those transects which have some data for trueherbs and also for env.
keep2<-!is.na(trueherbs[,1])&rowSums(trueherbs)>0&!is.na(env[,1])
keep2

#run a decorana and look at the axis length of axis 1. If it is less than 1.5, consider rda. If it is more than 2, consider ca/cca/dca.   Here it is 3.1394
trueherbsdca<-decorana(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])
trueherbsdca


# first we run a cca with all the environment data in it, just to have a look. 
ccatrueherbfull<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~Forest+Slope+Aspect+BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+pH+N.+C.+TCat+CanO+m.from.stream,env[keep2,])
ccatrueherbfull                            
anova(ccatrueherbfull)
# next we want to run ordistep: backward selection model, to get an idea which variables might have an influence. Ordistep needs a 'null' model with no environment data in (ie a ca) to compare the models it buils to. So we have to make that first. But we have to set it up in the same way as for a cca with envirnomental stuff in it. Ordistep returns the best model it can find to explain the data: the model it likes is in the output in the line that statrts 'Call:'
ccatrueherbs.null<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~1, data=env[keep2,])
ordistep(ccatrueherbs.null, scope=formula(ccatrueherbfull), perm.max = 1000)
# ordistep keeps LLcvov P= 0.040 Constrained   0.16129    0.06974 
   
#making a cca with just leaf cover as an explanatory variavble, then plotting the results
ccatrueherbs.llcov<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~LLCover,env[keep2,])
p<-plot(ccatrueherbs.llcov, display="sites", type="n", main="True herbs")

#this next step plots the sites and also lines to show the values of leaf litter cover (actually log(leaflittercover))
with(env[keep2,], ordisurf(ccatrueherbs.llcov, LLCover, nlevels=5, main="True herbs", col=1))
#plotting the sites with a different colour for each forest type
points(ccatrueherbs.llcov, display="sites", col=as.numeric(env$Forest[keep2])+1, pch=16)
points(ccatrueherbs.llcov, display="species", col=1, pch=1)
# this means you can click on the sites and it will put the transect number by them. Once you have clicked on as many sites as you like, rememeber the click stop in the top right hand side of the graphics window
identify(p, what="sites")
identify(p, what="species")
savePlot("cca trueherbs llcov.emf")

## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
# just forest
ccatrueherbjustforest<-cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~Forest,env[keep2,])
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
ccatrueherbs.FplusL<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~Forest+LLCover,env[keep2,])
ccatrueherbs.FplusL
#  Constrained    0.3792     0.1639
ccatrueherbs.FpartL<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~Forest+Condition(LLCover),env[keep2,])
ccatrueherbs.FpartL
# Constrained   0.21787    0.09421 
ccatrueherbs.LpartF<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~LLCover+Condition(Forest),env[keep2,])
ccatrueherbs.LpartF
#  Constrained   0.15865    0.06860 

anova(ccatrueherbs.FplusL)
# F3,27=1.765, P=0.025
colour<-c("black","grey15","lightgrey")
p<-plot(ccatrueherbs.FplusL, type="n")
with(env[keep2,], ordisurf(ccatrueherbs.FplusL, LLCover, main="Herbs", col=1, nlevels=6))
points(ccatrueherbs.FplusL, display="sites", col=colour[env$Forest[keep2]], pch=16)
legend(locator(1),c("og","os","ys"),pch=c(16,16,16),col=colour)
savePlot("cca herbs LLcov ordisurf.emf")




keep<-!is.na(env$Forest)&!is.na(trueherbs[,1])
# INDVAL - tells us which species are indicative of a particular forest type 
# import library
#library(labdsv) 
## select out samples where we know the forest age and the trueherb composition

# run indval and look at the results
#indvalsum<-indval ((trueherbs[keep,colSums(trueherbs[keep,])>0]), (env$Forest[keep]))
#summary(indvalsum  )
########         cluster indicator_value probability
########Marantochloa leucantha        3          0.4815       0.046
#if the adjusted version of env isn't already loaded....
#env$young_or_old<-ifelse(env$Forest =="ys", c("Recent"), c("Mature"))
#env$young_or_old<-as.factor(env$young_or_old) 
#env$og_or_sec<-ifelse(env$Forest =="og", c("OG"), c("Sec"))
#env$og_or_sec<-as.factor(env$og_or_sec) 
#str(env)
#indvalsum_young_or_old<-indval ((trueherbs[keep,colSums(trueherbs[keep,])>0]), (env$young_or_old[keep]))
#summary(indvalsum_young_or_old  )
#indvalsum_og_or_sec<-indval ((trueherbs[keep,colSums(trueherbs[keep,])>0]), (env$og_or_sec[keep]))
#summary(indvalsum_og_or_sec)


#correlation (point-biserial, a generalisation of phi for abundance data) instead of indval
library(indicspecies)

#herbstr<-strassoc(trueherbs[keep,], env$Forest[keep], func="r", nboot =1000)
#signassoc(trueherbs[keep,], env$Forest[keep], mode=1)  
#Error in matrix(1, nrow = nsps, ncol = ngroups): non-numeric matrix extent
herbpatt<-multipatt(trueherbs[keep,], env$Forest[keep], func="r")
summary(herbpatt)

#habit by ageclass
str(trueherbs)#31spp, 74sites inc na
str(ang_hab) 
env

#remember to run keep2 from line 4 above!!!!
temp<-merge(trueherbs[keep2,],env$Forest, by="row.names") 
str(temp)

trueherbYS<-temp[temp$y=="ys",2:32]
trueherbYS<-t(trueherbYS)
trueherbYS
YSstep1<-merge(ang_hab,trueherbYS, by.x="Species_Name", by.y="row.names", all=FALSE)
YSstep1$total<-rowSums(YSstep1[,5:18]>0) 
YSstep1
YSsums<-xtabs(total~Habitat, data=YSstep1)
YSsums

trueherbOS<-temp[temp$y=="os",2:32]
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
OGstep1$total<-rowSums(OGstep1[,5:12]>0) 
OGstep1
OGsums<-xtabs(total~Habitat, data=OGstep1)
OGsums

sums<-cbind(YSsums, OSsums, OGsums)
 sums


barplot(sums, ylim=c(0,150), xlab="Age class", ylab="Sum of occurrences of each affiliation group", legend=c("F", "FF", "G"), main="Herbs")
savePlot("Herb_affiliations.emf", type="emf")

