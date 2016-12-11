source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

# 'keep2' is a grid of true/false values that selects those columns of 'trueherbs' that have at least one ocurrence in them (!is.na means 'is not na'), and those transects which have some data for trueherbs and also for env.
keep2<-!is.na(trueherbs[,1])&rowSums(trueherbs)>0&!is.na(env[,1])
keep2

#run a decorana and look at the axis length of axis 1. If it is less than 1.5, consider rda. If it is more than 2, consider ca/cca/dca. 
trueherbsdca<-decorana(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])
trueherbsdca  #3.19


# first we run a rda with all the environment data in it, just to have a look. 
rdatrueherbfull<-rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~newage+Slope+Aspect+BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+pH+N.+C.+TCat+CanO+m.from.stream,env[keep2,])
rdatrueherbfull  #76%                          
anova(rdatrueherbfull)#NS
# next we want to run ordistep: backward selection model, to get an idea which variables might have an influence. Ordistep needs a 'null' model with no environment data in (ie a pca) to compare the models it buils to. So we have to make that first. But we have to set it up in the same way as for a rda with envirnomental stuff in it. Ordistep returns the best model it can find to explain the data: the model it likes is in the output in the line that statrts 'Call:'
rdatrueherbs.null<-rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~1, data=env[keep2,])
rdatrueherbs.null
ordistep(rdatrueherbs.null, scope=formula(rdatrueherbfull), perm.max = 1000)
# full ordistep keeps TCAT! and LLcov 

## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
# just forest
rdatrueherbjustforest<-rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~newage,env[keep2,])
anova(rdatrueherbjustforest)
rdatrueherbjustforest
# no need for ordistep here; anova says model is ns, Constrained   9.5%
#now the structure variables
rdatrueherbstruc<-rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep2,])
ordistep(rdatrueherbs.null, scope=formula(rdatrueherbstruc), perm.max = 1000)
#chooses null
#soils
rdatrueherbsoil<-rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~pH+N.+C.+TCat,env[keep2,])
ordistep(rdatrueherbs.null, scope=formula(rdatrueherbsoil), perm.max = 1000)
# chooses null model
#now the topo variables
rdatrueherbtopo<-rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~Slope+Aspect+m.from.stream,env[keep2,])
ordistep(rdatrueherbs.null, scope=formula(rdatrueherbtopo), perm.max = 1000)
#chooses null model

# then all the combinations of forest and significant variables (from help file: the following commands are equivalent: rda(X, Y, Z), rda(X ~ Y + Condition(Z)), where Y and Z refer to constraints and conditions matrices respectively. )
rdatrueherbs.FplusL<- rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~newage+LLCover,env[keep2,])
rdatrueherbs.FplusL
vif.rda(rdatrueherbs.FplusL)
 
rdatrueherbs.LpartF<- rda(sqrt(decostand(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1],method="total"))~LLCover+Condition(newage),env[keep2,])
rdatrueherbs.LpartF   #  Constrained  
anova(rdatrueherbs.FplusL)   #


colour<-c("black","grey15","lightgrey")
p<-plot(rdatrueherbs.FplusL, type="n")
with(env[keep2,], ordisurf(rdatrueherbs.FplusL, LLCover, main="Herbs", col=1, nlevels=6))
points(rdatrueherbs.FplusL, display="sites", col=colour[env$newage[keep2]], pch=16)
legend(locator(1),c("og","os","ys"),pch=c(16,16,16),col=colour)
savePlot("rda herbs LLcov ordisurf.emf")
