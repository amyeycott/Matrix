source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
keep3<-!is.na(ferns[,1])&rowSums(ferns)>0&!is.na(env[,1])
keep2<-!is.na(trueherbs[,1])&rowSums(trueherbs)>0&!is.na(env[,1])
keep6<-!is.na(subcanopywoody[,1])&rowSums(subcanopywoody)>0&!is.na(env[,1])
keep5<-!is.na(trees[,1])&rowSums(trees)>0&!is.na(env[,1])
ccafernmodel<-cca(ferns[keep3,colSums(ferns>0, na.rm=T)>1]~newage+pH,env[keep3,])
ccatrueherbs.FplusL<- cca(sqrt(trueherbs[keep2,colSums(trueherbs>0, na.rm=T)>1])~newage+LLCover,env[keep2,])
ccasubcanopywoody.FplusTCat<- cca(sqrt(subcanopywoody[keep6,colSums(subcanopywoody>0, na.rm=T)>1])~newage+TCat+LLDepth,env[keep6,])
ccabigtrees.FplusTCat<- cca(trees[keep5,colSums(trees>0, na.rm=T)>1]~newage+TCat,env[keep5,])

x11();par(mfrow=c(2,2), mar=c(4,3,1,1), mgp=c(2,0.8,0), cex.main=1)
colour<-c("black","grey35","lightgrey")
p<-plot(ccabigtrees.FplusTCat, type="n")
points(ccabigtrees.FplusTCat, display="sites", col=colour[env$newage[keep5]], pch=16)
with(env[keep5,], ordisurf(ccabigtrees.FplusTCat, TCat, nlevels=3, main="Trees > 10 cm dbh", col=1, add=T))
legend("topleft",c("Younger secondary","Mid- secondary","Older growth"),pch=c(16,16,16),col=sort(colour, decreasing=T), bg="white")
plot(ccasubcanopywoody.FplusTCat, type="n")
points(ccasubcanopywoody.FplusTCat, display="sites", col=colour[env$newage[keep6]], pch=16)
with(env[keep6,], ordisurf(ccasubcanopywoody.FplusTCat, TCat, nlevels=3, main="Sub-canopy woody plants", col=1, add=T))
p<-plot(ccatrueherbs.FplusL, type="n")
points(ccatrueherbs.FplusL, display="sites", col=colour[env$newage[keep2]], pch=16)
with(env[keep2,], ordisurf(ccatrueherbs.FplusL, LLCover, main="Herbs", col=1, nlevels=6, add=T))
p<-plot(ccafernmodel, type="n", xlim=c(-2,3))
points(ccafernmodel, display="sites", col=colour[env$newage[keep3]], pch=16)
with(env[keep3,], ordisurf(ccafernmodel, pH, main="Ferns", nlevels=3, col=1, add=T)) #picked the stronger variable



savePlot("Figure 4 Mass CCA NEW AGE.emf", type="emf")
savePlot("Figure 4 Eycott et al CCA.eps", type="eps")


plot(ccabigtrees.FplusTCat) #57
plot(ccasubcanopywoody.FplusTCat)    #33 from os, 57 from OG
plot(ccatrueherbs.FplusL) #4 from ys, 57 from OG
plot(ccafernmodel)
env[c(27,25),]  #27 and 25 from ys
