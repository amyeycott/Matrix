source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
ls()
keep3<-!is.na(fernabun[,1])&rowSums(fernabun)>0&!is.na(env[,1])
keep3

#axis length test
dcafernempty<-decorana(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="total"))) #5.86!

rdafernfull<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="total"))~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+pH+N.+C.+TCat+CanO+m.from.stream,env[keep3,])
rdafernfull  #37% explained constrained
anova(rdafernfull)#**
plot(rdafernfull)

rdafernempty<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="total"))~1, data=env[keep3,])
ordistep(rdafernempty, scope=formula(rdafernfull), perm.max = 1000)
## straight ordistep reports m from stream, newage and LLCOV. Constrained explains 16%

## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
rdafernsoil<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~pH+N.+C.+TCat,env[keep3,])
ordistep(rdafernempty, scope=formula(rdafernsoil), perm.max = 1000) #keeps empty model

#now the structure variables
rdafernstruc<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep3,])
ordistep(rdafernempty, scope=formula(rdafernstruc), perm.max = 1000) #keeps deadwood
rdafernjustdeadwood<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~Deadwood,env[keep3,]) 
rdafernjustdeadwood  #4.1%
anova(rdafernjustdeadwood) #**

#now the topo variables
rdaferntopo<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~Slope+m.from.stream,env[keep3,])
ordistep(rdafernempty, scope=formula(rdaferntopo), perm.max = 1000)#keeps m. from stream
rdafernjustmstream<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~m.from.stream,env[keep3,])
rdafernjustmstream # 4.9%
anova(rdafernjustmstream)  #P=0.015

  
#now just forest
rdafernjustforest<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~newage,env[keep3,])
anova(rdafernjustforest)#**
rdafernjustforest #7.1%
# no need for ordistep here; constrained prop7.1%**

#final model: first test whether it's full
rdafernmodel<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~newage+m.from.stream+Deadwood,env[keep3,])
rdafernmodel #Constrained  0.151
vif.cca(rdafernmodel) #highest is 2.13 (forestOS)
anova(rdafernmodel)

rdafernmodelpart<-rda(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~m.from.stream+Deadwood+Condition(newage),env[keep3,])
rdafernmodelpart
anova(rdafernmodelpart)
#Constrained  7.9%, conditoinal 7.1% 

p<-plot(rdafernmodel, type="n")
colour<-c("black","grey35","grey65")
with(env[keep3,], ordisurf(rdafernmodel, m.from.stream, main="Ferns", nlevels=3, col=1))
points(rdafernmodel, display="sites", col=colour[env$newage[keep3]], pch=16)
legend(locator(1),c("OG","MS","YS"),pch=c(16,16,16),col=colour)
savePlot("fernrda STAND with mfromstream ordisurf.emf")

