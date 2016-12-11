source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

keep5<-!is.na(trees[,1])&rowSums(trees)>0&!is.na(env[,1])
keep5

#big trees is counts. The DCA is to check axis length, which was plenty long enough!
dcabigtrees<-decorana(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1], method="total")))
dcabigtrees #4.43 length axis 1

ccabigtreesfull<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+pH+N.+C.+TCat+CanO+m.from.stream,env[keep5,])
ccabigtreesfull
anova(ccabigtreesfull) #56%*

ccabigtrees.null<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~1, data=env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreesfull), perm.max = 1000)
# ordistep chooses newage, TCat & pH.


## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
ccabigtreesoil<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~pH+N.+C.+TCat,env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreesoil), perm.max = 1000) # chooses TCat and pH
ccabigtreetcatpH<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~TCat+pH,env[keep5,])
ccabigtreetcatpH
anova(ccabigtreejusttcat)   #14.6%**

#now the structure variables
ccabigtreestruc<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreestruc), perm.max = 1000)
# chooses null model
#now the topo variables
ccabigtreestopo<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~Slope+m.from.stream,env[keep5,])
ordistep(ccabigtrees.null, scope=formula(ccabigtreestopo), perm.max = 1000)
#keeps  null model
#now just forest
ccabigtreesjustforest<-rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~newage,env[keep5,])
anova(ccabigtreesjustforest)
ccabigtreesjustforest
# no need for ordistep here; anova says model is sig 
ccabigtrees.FplusTCatpH<- rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~newage+TCat+pH,env[keep5,])
ccabigtrees.FplusTCatpH
anova(ccabigtrees.FplusTCatpH)
#   Constrained  
ccabigtreespart<- rda(sqrt(decostand(trees[keep5,colSums(trees>0, na.rm=T)>1],method="total"))~TCat+pH+Condition(newage),env[keep5,])
ccabigtreespart
anova(ccabigtreespart)
#  Constrained  
vif.cca(ccabigtrees.FplusTCatpH)    #highest 3.03 newageys