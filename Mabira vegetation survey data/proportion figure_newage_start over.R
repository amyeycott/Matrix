source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")

#This is different to the original in several ways, of which the first two are most notable. Firstly, it works with ocurrences not just divisions of the species list. Second, it starts with cbind because merge was not handling NA correctly and so producing messed up row indexing. Third, keep now includes sites with no species in. Fourth, this file edits ang_hab and fern_hab to have dots instead of spaces in species names.  Fifth, the column selections when temp is divided into YS, OS and OG are different here - they start at 1 and are less magic-numbery.


keep5x<-!is.na(trees[,1])&!is.na(env[,1])
keep6x<-!is.na(subcanopywoody[,1])&!is.na(env[,1])
keep2x<-!is.na(trueherbs[,1])&!is.na(env[,1])
keep3x<-!is.na(ferns[,1])&!is.na(env[,1])
ang_hab$Species_Name<-gsub(" ",".", ang_hab$Species_Name)
rownames(fern_hab)<-gsub(" ",".", rownames(fern_hab))


temp.trees<-data.frame(cbind(trees[keep5x,], env$newage[keep5x]))  
bigtreesYS<-temp.trees[temp.trees$env.newage.keep5x.=="ys",1:ncol(temp.trees)-1] 
bigtreesYS<-t(bigtreesYS)
YSstep1.trees<-merge(ang_hab,bigtreesYS, by.x=1, by.y=0, all=FALSE)
bigtreesOS<-temp.trees[temp.trees$env.newage.keep5x.=="os",1:ncol(temp.trees)-1] 
bigtreesOS<-t(bigtreesOS)
OSstep1.trees<-merge(ang_hab,bigtreesOS, by.x="Species_Name", by.y="row.names", all=FALSE)
bigtreesOG<-temp.trees[temp.trees$env.newage.keep5x.=="og",1:ncol(temp.trees)-1] 
bigtreesOG<-t(bigtreesOG)
OGstep1.trees<-merge(ang_hab,bigtreesOG, by.x="Species_Name", by.y="row.names", all=FALSE)
OG.trees.table<-table(OGstep1.trees$Habitat[rowSums(OGstep1.trees[5:ncol(OGstep1.trees)])>0])

trees.forfig<-data.frame(rbind(OG=table(OGstep1.trees$Habitat[rowSums(OGstep1.trees[5:ncol(OGstep1.trees)])>0]),MS=table(OSstep1.trees$Habitat[rowSums(OSstep1.trees[5:ncol(OSstep1.trees)])>0]),YS=table(YSstep1.trees$Habitat[rowSums(YSstep1.trees[5:ncol(YSstep1.trees)])>0])))
chisq.test(trees.forfig[1:2])

temp.scw<-data.frame(cbind(subcanopywoody[keep6x,], env$newage[keep6x])) 
subcanopywoodyYS<-temp.scw[temp.scw$env.newage.keep6x.=="ys",1:ncol(temp.scw)-1]
subcanopywoodyYS<-t(subcanopywoodyYS)
YSstep1.scw<-merge(ang_hab,subcanopywoodyYS, by.x="Species_Name", by.y="row.names", all=FALSE)
subcanopywoodyOS<-temp.scw[temp.scw$env.newage.keep6x.=="os",1:ncol(temp.scw)-1]
subcanopywoodyOS<-t(subcanopywoodyOS)
OSstep1.scw<-merge(ang_hab,subcanopywoodyOS, by.x="Species_Name", by.y="row.names", all=FALSE)
subcanopywoodyOG<-temp.scw[temp.scw$env.newage.keep6x. =="og",1:ncol(temp.scw)-1]
subcanopywoodyOG<-t(subcanopywoodyOG)
OGstep1.scw<-merge(ang_hab,subcanopywoodyOG, by.x="Species_Name", by.y="row.names", all=FALSE)

scw.forfig<-data.frame(rbind(OG=table(OGstep1.scw$Habitat[rowSums(OGstep1.scw[5:ncol(OGstep1.scw)])>0]),MS=table(OSstep1.scw$Habitat[rowSums(OSstep1.scw[5:ncol(OSstep1.scw)])>0]),YS=table(YSstep1.scw$Habitat[rowSums(YSstep1.scw[5:ncol(YSstep1.scw)])>0])))
chisq.test(scw.forfig[1:2])


temp.herbs<-data.frame(cbind(trueherbs[keep2x,], env$newage[keep2x])) 
trueherbYS<-temp.herbs[temp.herbs$env.newage.keep2x.=="ys",1:ncol(temp.herbs)-1]
trueherbYS<-t(trueherbYS)
YSstep1.herbs<-merge(ang_hab,trueherbYS, by.x="Species_Name", by.y="row.names", all=FALSE)
trueherbOS<-temp.herbs[temp.herbs$env.newage.keep2x.=="os",1:ncol(temp.herbs)-1]
trueherbOS<-t(trueherbOS)
OSstep1.herbs<-merge(ang_hab,trueherbOS, by.x="Species_Name", by.y="row.names", all=FALSE)
trueherbOG<-temp.herbs[temp.herbs$env.newage.keep2x.=="og",1:ncol(temp.herbs)-1]
trueherbOG<-t(trueherbOG)
OGstep1.herbs<-merge(ang_hab,trueherbOG, by.x="Species_Name", by.y="row.names", all=FALSE)

herbs.forfig<-data.frame(rbind(OG=table(OGstep1.herbs$Habitat[rowSums(OGstep1.herbs[5:ncol(OGstep1.herbs)])>0]),MS=table(OSstep1.herbs$Habitat[rowSums(OSstep1.herbs[5:ncol(OSstep1.herbs)])>0]),YS=table(YSstep1.herbs$Habitat[rowSums(YSstep1.herbs[5:ncol(YSstep1.herbs)])>0])))
chisq.test(herbs.forfig[1:2])

temp.f<-data.frame(cbind(ferns[keep3x,], env$newage[keep3x]))
###merge(ferns.keep3x,env.keep3x$newage, by="row.names") #merge broke my data frame
fernsYS<-temp.f[temp.f$env.newage.keep3x.=="ys",1:ncol(temp.f)-1]
fernsYS<-t(fernsYS)
YSstep1.ferns<-merge(fern_hab,fernsYS, by.x=0, by.y=0, all=FALSE)
fernsOS<-temp.f[temp.f$env.newage.keep3x.=="os",1:ncol(temp.f)-1]
fernsOS<-t(fernsOS)
OSstep1.ferns<-merge(fern_hab,fernsOS, by.x=0, by.y=0, all=FALSE)
fernsOG<-temp.f[temp.f$env.newage.keep3x.=="og",1:ncol(temp.f)-1]
fernsOG<-t(fernsOG)
OGstep1.ferns<-merge(fern_hab,fernsOG, by.x=0, by.y=0, all=FALSE)

ferns.forfig<-data.frame(rbind(OG=table(OGstep1.ferns$Habitat[rowSums(OGstep1.ferns[5:ncol(OGstep1.ferns)])>0]),MS=table(OSstep1.ferns$Habitat[rowSums(OSstep1.ferns[5:ncol(OSstep1.ferns)])>0]),YS=table(YSstep1.ferns$Habitat[rowSums(YSstep1.ferns[5:ncol(YSstep1.ferns)])>0])))
ferns.forfig
chisq.test(ferns.forfig[1:2])

coloury=c("grey 20","grey 57", "grey 87")
x11(5.5,5.5); par(mfrow=c(2,2), mar=c(3.5,3.5,3.5,1),xpd=NA)
barplot(t(trees.forfig/rowSums(trees.forfig)), xlab="Forest type", ylab="Proportion of ocurrences", main="Trees >10 cm dbh", col=coloury, las=1, font.main=1,mgp=c(2,0.3,0), tcl=-0.2)
barplot(t(scw.forfig/rowSums(scw.forfig)), xlab="Forest type", ylab="Proportion of ocurrences", main="Sub-canopy woody plants" , col=coloury, las=1, font.main=1,mgp=c(2,0.3,0), tcl=-0.2)
barplot(t(herbs.forfig/rowSums(herbs.forfig)), xlab="Forest type", ylab="Proportion of ocurrences", main="Herbs", col=coloury, las=1, font.main=1,mgp=c(2,0.3,0), tcl=-0.2)
barplot(t(ferns.forfig/rowSums(ferns.forfig)), xlab="Forest type", ylab="Proportion of ocurrences", main="Ferns" , col=coloury, las=1, font.main=1,mgp=c(2,0.3,0), tcl=-0.2)
legend(x=-2.3, y=1.55, legend=c("Forest specialists","Forest generalists", "Open habitat species"), fill=coloury) 
savePlot("Figure 3 Barplots.eps", type="eps")
savePlot("Figure 3 Barplots.pdf", type="pdf")

