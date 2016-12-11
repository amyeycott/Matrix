source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
ls()
keep3<-!is.na(fernabun[,1])&rowSums(fernabun)>0&!is.na(env[,1])
keep3

#axis length test
dcafernempty<-decorana(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="total"))) #5.86!

#this makes chord distance
capscalefernfull<-capscale(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="norm")~newage+Slope+BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+pH+N.+C.+TCat+CanO+m.from.stream,env[keep3,], dist="euclid")
capscalefernfull  #38% explained constrained
anova(capscalefernfull)#**
plot(capscalefernfull)

testdist<-vegdist(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="norm"), dist="euclid")


capscalefernempty<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1],method="total"))~1, data=,env[keep3,], dist="bray")
ordistep(capscalefernempty, scope=formula(capscalefernfull), perm.max = 1000)
## straight ordistep reports m from stream, newage and LLCOV. Constrained explains 16%

## variance partitioning steps: first, choose one variable from each group forest, structure, soils, topography using ordistep
capscalefernsoil<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~pH+N.+C.+TCat,env[keep3,], dist="bray")
capscalefernsoil
ordistep(capscalefernempty, scope=formula(capscalefernsoil), perm.max = 1000) #keeps empty model

#now the structure variables
capscalefernstruc<-capscale(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total")~BA+Deadwood+Stumps+LLCover+LLDepth+ShrubCover+CanO,env[keep3,], dist="chord")



ordistep(capscalefernempty, scope=formula(capscalefernstruc), perm.max = 1000) #keeps deadwood
capscalefernjustdeadwood<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~Deadwood,env[keep3,], dist="bray") 
capscalefernjustdeadwood  #4.1%
anova(capscalefernjustdeadwood) #**

#now the topo variables
capscaleferntopo<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~Slope+m.from.stream,env[keep3,], dist="bray")
ordistep(capscalefernempty, scope=formula(capscaleferntopo), perm.max = 1000)#keeps m. from stream
capscalefernjustmstream<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~m.from.stream,env[keep3,], dist="bray")
capscalefernjustmstream # 4.9%
anova(capscalefernjustmstream)  #P=0.015

  
#now just forest
capscalefernjustforest<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~newage,env[keep3,], dist="bray")
anova(capscalefernjustforest)#**
capscalefernjustforest #7.1%
# no need for ordistep here; constrained prop7.1%**

#final model: first test whether it's full
capscalefernmodel<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~newage+m.from.stream+Deadwood,env[keep3,], dist="bray")
capscalefernmodel #Constrained  0.151
vif.cca(capscalefernmodel) #highest is 2.13 (forestOS)
anova(capscalefernmodel)

capscalefernmodelpart<-capscale(sqrt(decostand(fernabun[keep3,colSums(fernabun>0, na.rm=T)>1], method="total"))~m.from.stream+Deadwood+Condition(newage),env[keep3,], dist="bray")
capscalefernmodelpart
anova(capscalefernmodelpart)
#Constrained  7.9%, conditoinal 7.1% 

p<-plot(capscalefernmodel, type="n")
colour<-c("black","grey35","grey65")
with(env[keep3,], ordisurf(capscalefernmodel, m.from.stream, main="Ferns", nlevels=3, col=1))
points(capscalefernmodel, display="sites", col=colour[env$newage[keep3]], pch=16)
legend(locator(1),c("OG","MS","YS"),pch=c(16,16,16),col=colour)
savePlot("ferncapscale STAND with mfromstream ordisurf.emf")

