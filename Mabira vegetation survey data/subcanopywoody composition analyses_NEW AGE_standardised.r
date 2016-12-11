source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

### this file uses both the trees below 10cm dbh, and the shrubs.
keep6<-!is.na(subcanopywoody[,1])&rowSums(subcanopywoody)>0&!is.na(env[,1])
keep6
dcasubcanopywoodyfull<-decorana(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1]))
dcasubcanopywoodyfull #axis 1 4.24
plot(dcasubcanopywoodyfull)

#note that shrub cover is removed, unlike for the herbs and ferns, because the shrubs are in the ordination.
rdasubcanopywoodyfull<-rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+pH+N.+C.+TCat+CanO+m.from.stream,env[keep6,])
rdasubcanopywoodyfull
anova(rdasubcanopywoodyfull)    #49% NS

rdawithnothingin<-rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~1, data=env[keep6,])
ordistep(rdawithnothingin, scope=formula(rdasubcanopywoodyfull), perm.max = 1000)
# full ordistep keeps nothing!


## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
rdasubcanopywoodysoil<-rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~pH+N.+C.+TCat,env[keep6,])
ordistep(rdawithnothingin, scope=formula(rdasubcanopywoodysoil), perm.max = 1000)
# chooses  empty , but is unstable and sometimes chooses TCAT
#now the structure variables - note shrub cover is removed!
rdasubcanopywoodystruc<-rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~BA+Deadwood+Stumps+LLCover+LLDepth+CanO,env[keep6,])
ordistep(rdawithnothingin, scope=formula(rdasubcanopywoodystruc), perm.max = 1000) #chooses empty
#now the topo variables
rdasubcanopywoodytopo<-rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~Slope+m.from.stream,env[keep6,])
ordistep(rdawithnothingin, scope=formula(rdasubcanopywoodytopo), perm.max = 1000)
# chooses null model 
#now just forest
rdasubcanopywoodyjustforest<-rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~newage,env[keep6,])
anova(rdasubcanopywoodyjustforest)
rdasubcanopywoodyjustforest
# no need for ordistep here; anova says is not significant, constrained 10.3%

#stopped here because of emptiness ##########
# then all the combinations 
rdasubcanopywoody.FplusTCatplusLLDepth<- rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~newage+TCat+LLDepth,env[keep6,])
rdasubcanopywoody.FplusTCatplusLLDepth
vif.rda(rdasubcanopywoody.FplusTCatplusLLDepth)
# Constrained    
#rdasubcanopywoody.FpartTCat<- rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~newage+Condition(TCat),env[keep6,])
rdasubcanopywoody.FpartTCat
# Constrained  
rdasubcanopywoody.TCatplusLLDepthpartF<- rda(sqrt(decostand(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1], method="total"))~TCat+LLDepth+Condition(newage),env[keep6,])
rdasubcanopywoody.TCatplusLLDepthpartF
# Constrained   

colour<-c("black","grey15","lightgrey")
anova(rdasubcanopywoody.FplusTCatplusLLDepth)
# F4,26=1.256, P 0 0.005
plot(rdasubcanopywoody.FplusTCatplusLLDepth, type="n")
with(env[keep6,], ordisurf(rdasubcanopywoody.FplusTCatplusLLDepth, TCat, nlevels=5, main="Sub-canopy woody plants", col=1, labcex=1.2))
points(rdasubcanopywoody.FplusTCatplusLLDepth, display="sites", col=colour[env$newage[keep6]], pch=16)

legend(locator(1),c("og","os","ys"),pch=c(16,16,16),col=colour)
savePlot("treeseedling rda with ordisurf.emf")

