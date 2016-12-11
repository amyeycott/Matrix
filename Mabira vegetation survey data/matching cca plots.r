source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
keep3<-!is.na(ferns[,1])&rowSums(ferns)>0&!is.na(env[,1])
keep2<-!is.na(trueherbs[,1])&rowSums(trueherbs)>0&!is.na(env[,1])
keep6<-!is.na(subcanopywoody[,1])&rowSums(subcanopywoody)>0&!is.na(env[,1])
keep5<-!is.na(trees[,1])&rowSums(trees)>0&!is.na(env[,1])
ccafernmodel<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~Forest+pH,env[keep3,])
ccatrueherbs.FplusL<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~Forest+LLCover,env[keep2,])
ccasubcanopywoody.FplusTCat<- cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~Forest+TCat+LLDepth,env[keep6,])
ccabigtrees.FplusTCat<- cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~Forest+TCat,env[keep5,])

x11();par(mfrow=c(2,2), mar=c(4,3,1,1), mgp=c(2,0.8,0), cex.main=1)
colour<-c("black","grey35","lightgrey")
p<-plot(ccabigtrees.FplusTCat, type="n")
with(env[keep5,], ordisurf(ccabigtrees.FplusTCat, TCat, nlevels=3, main="Trees >10cm dbh", col=1, add=T))
points(ccabigtrees.FplusTCat, display="sites", col=colour[env$Forest[keep5]], pch=16)
plot(ccasubcanopywoody.FplusTCat, type="n")
with(env[keep6,], ordisurf(ccasubcanopywoody.FplusTCat, TCat, nlevels=5, main="Sub-canopy woody plants", col=1, add=T))
points(ccasubcanopywoody.FplusTCat, display="sites", col=colour[env$Forest[keep6]], pch=16)
p<-plot(ccatrueherbs.FplusL, type="n")
with(env[keep2,], ordisurf(ccatrueherbs.FplusL, LLCover, main="Herbs", col=1, nlevels=6, add=T))
points(ccatrueherbs.FplusL, display="sites", col=colour[env$Forest[keep2]], pch=16)
legend("bottomright",c("Young secondary","Old secondary","Old growth"),pch=c(16,16,16),col=sort(colour, decreasing=T))
p<-plot(ccafernmodel, type="n", xlim=c(-2,3))
with(env[keep3,], ordisurf(ccafernmodel, pH, main="Ferns", nlevels=3, col=1, add=T)) #picked the stronger variable
points(ccafernmodel, display="sites", col=colour[env$Forest[keep3]], pch=16)




savePlot("Mass CCA.emf", type="emf")