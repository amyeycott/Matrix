source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Budongo\\moreen fragments load data.r")

#Here are the spotplots in threes with range bars (some in colour, some not) for ind transects. They run till line 77-ish. Figure 2 is about line 166, Figure 4 is 209

x11(); par(mar=c(5,4,1,1), mfrow=c(2,2), xpd=NA)
errbar(sums$Distance, sums$spprich, maxes$spprich, mins$spprich, ylab="Total number species", ylim=c(0,40), xlim=c(0,15), col= c("gray27", "red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)", errbar.col=c("gray27", "red","gray57","gray87")[as.factor(sums$Matrix.type)])
errbar(sums$FragmentSize, sums$spprich, maxes$spprich, mins$spprich, ylab="Total number species", ylim=c(0,40), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Fragment size (ha)",errbar.col=c("gray27", "red","gray57","gray87")[as.factor(sums$Matrix.type)])
arrows(460,sums$spprich[sums$Group.1=="RP7"], 500,sums$spprich[sums$Group.1=="RP7"], length=0.08, lwd=2,col="red") 
errbar(sums$V1, sums$spprich, maxes$spprich, mins$spprich, ylab="Total number species", ylim=c(0,40), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Canopy Openness (%)", errbar.col=c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)])
legend(x=38, y=40,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots ordinary richness ranges.eps", type="eps")

x11(); par(mar=c(5,4,1,1), mfrow=c(2,2), xpd=NA)
plot(sums$rarefied~sums$Distance, ylab="Rarefied species richness", ylim=c(0,45), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Distance from forest (km)")
errbar(sums$Distance, sums$rarefied, maxes$rarefied, mins$rarefied, add=T, type="n")
plot(sums$rarefied~sums$FragmentSize, ylab="Rarefied species richness", ylim=c(0,45), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Fragment size (ha)")
errbar(sums$FragmentSize, sums$rarefied, maxes$rarefied, mins$rarefied, add=T, type="n")
arrows(460,sums$rarefied[sums$Group.1=="RP7"], 500,sums$rarefied[sums$Group.1=="RP7"], length=0.08, lwd=2,col="red")
plot(sums$rarefied~sums$V1, ylab="Rarefied species richness", ylim=c(0,45), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")
errbar(sums$V1, sums$rarefied, maxes$rarefied, mins$rarefied, add=T, type="n")
legend(x=38, y=40,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots rarefied richness ranges.eps", type="eps")

x11(); par(mar=c(5,4,1,1), mfrow=c(2,2), xpd=NA)
plot(sums$abun~sums$Distance, ylab="Bird abundance", ylim=c(0,300), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Distance from forest (km)")
errbar(sums$Distance, sums$abun, maxes$totalbirds, mins$totalbirds, add=T, type="n")
plot(sums$abun~sums$FragmentSize, ylab="Bird abundance", ylim=c(0,300), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Fragment size (ha)")
errbar(sums$FragmentSize, sums$abun, maxes$totalbirds, mins$totalbirds, add=T, type="n")
arrows(460,sums$abun[sums$Group.1=="RP7"], 500,sums$abun[sums$Group.1=="RP7"], length=0.08, lwd=2,col="red")
plot(sums$abun~sums$V1, ylab="Bird abundance", ylim=c(0,300), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")
errbar(sums$V1, sums$abun, maxes$totalbirds, mins$totalbirds, add=T, type="n")
legend(x=38, y=250,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots abundance ranges.eps", type="eps")

x11(); par(mar=c(5,4,2,1), mfrow=c(2,2), xpd=NA)
plot((sums$FF/sums$totalbirds)~sums$Distance, ylab="Proportion individuals FF", ylim=c(0,1), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Distance from forest (km)")
errbar(sums$Distance,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, add=T, type="n")
plot((sums$FF/sums$totalbirds)~sums$FragmentSize, ylab="Proportion individuals FF", ylim=c(0,1), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=F, xlab="Fragment size (ha)")
errbar(sums$FragmentSize,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, add=T, type="n")
arrows(460,(sums$FF[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), 500,(sums$FF[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="red")
plot((sums$FF/sums$totalbirds)~sums$V1, ylab="Proportion individuals FF", ylim=c(0,1), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")
errbar(sums$V1,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, add=T, type="n")
legend(x=38, y=0.9,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots FFpropindiv ranges.eps", type="eps")

x11(); par(mar=c(5,4,1,1), mfrow=c(2,2), xpd=NA)
plot((sums$INGR/sums$totalbirds)~sums$Distance, ylab="Proportion individuals GRIN", ylim=c(0,0.4), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)")
errbar(sums$Distance,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, add=T, type="n")
plot((sums$INGR/sums$totalbirds)~sums$FragmentSize, ylab="Proportion individuals GRIN", ylim=c(0,0.4), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=F, xlab="Fragment size (ha)")
errbar(sums$FragmentSize,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, add=T, type="n")
arrows(460,(sums$INGR[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), 500,(sums$INGR[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="red")
plot((sums$INGR/sums$totalbirds)~sums$V1, ylab="Proportion individuals GRIN", ylim=c(0,0.4), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")
errbar(sums$V1,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, add=T, type="n")
legend(x=38, y=0.4,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots INGRpropindiv ranges.eps", type="eps")

#gleaning insectivores: most species. Note switch to more elegant code
x11(); par(mar=c(5,4,2,1), mfrow=c(2,2), xpd=NA)
errbar(sums$Distance,(sums$INGLL/sums$totalbirds) , maxes$INGLLprop, mins$INGLLprop, ylim=c(0,0.4), xlim=c(0,15), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion individuals INGLL", xlab="Distance from forest (km)")
errbar(sums$FragmentSize,(sums$INGLL/sums$totalbirds) , maxes$INGLLprop, mins$INGLLprop, ylim=c(0,0.4), xlim=c(0,500), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion individuals INGLL", xlab="Fragment size (ha)")
arrows(460,(sums$INGLL[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), 500,(sums$INGLL[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="red")
errbar(sums$V1,(sums$INGLL/sums$totalbirds) , maxes$INGLLprop, mins$INGLLprop, ylim=c(0,0.4), xlim=c(0,25), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion individuals INGLL", xlab="Canopy Openness (%)")
legend(x=40, y=0.4,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red")) 
savePlot("threespotplots INGLL propindiv ranges.emf")

#frugivores - most tested by other people
x11(); par(mar=c(5,4,2,1), mfrow=c(2,2), xpd=NA)
errbar(sums$Distance,(sums$FR/sums$totalbirds) , maxes$FRprop, mins$FRprop, ylim=c(0,0.5), xlim=c(0,15), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of frugivores", xlab="Distance from forest (km)")
errbar(sums$FragmentSize,(sums$FR/sums$totalbirds) , maxes$FRprop, mins$FRprop, ylim=c(0,0.5), xlim=c(0,500), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of frugivores", xlab="Fragment size (ha)")
arrows(460,(sums$FR[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), 500,(sums$FR[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="red")
errbar(sums$V1,(sums$FR/sums$totalbirds) , maxes$FRprop, mins$FRprop, ylim=c(0,0.5), xlim=c(0,25), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of frugivores", xlab="Canopy Openness (%)")
legend(x=40, y=0.4,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red")) 
savePlot("threespotplots FR propindiv ranges.emf")


# there is a confounding between distance and marix type (intensive is further), between fragment size and matrix type is ok, between canopyopeness and matrix type the two intensive matrixes have more open canopies than bushland and cont forest do. Canopy openness by fragment size is probably ok too.
env
x11(); par(mar=c(4,4,1,1))
plot(Distance~Matrix.type, data=env[env$Transect=="1",])
plot(FragmentSize~Matrix.type, data=env[env$Transect=="1",])
plot(log(CanopyOpeness)~Matrix.type, data=env)
plot(log(CanopyOpeness)~FragmentSize, data=env[env$Transect=="1",], col=as.factor(Matrix.type) )
plot(log(CanopyOpeness)~Distance, data=env[env$Transect=="1",], col= c("gray57","gray7","gray27","gray87")[as.factor(Matrix.type)], pch=16, cex=1.5)
legend("topleft",c("Bushland","Farmland","Sugarcane", "Continuous Forest"),pch=15,col=c("gray27","gray57","gray87","gray7"))

#stacked barplots of guilds by n indiv then by n spp

x11();par(mar=c(10,4,4,1), xpd=NA, mfrow=c(1,2))   
barplot(as.matrix(t(sums[,c("FF","F","FV","NF","O")])), ylim=c(0,300), names.arg=c("Busaju:Bushland","Tengele:Bushland", "Ongo:Farmland","Rwesama:Farmland","Kasokwa:Sugarcane","Kinyara:Sugarcane","RP7:Continuous Forest"), col=c("gray7","gray27","gray47","gray67","gray87"), las=2, ylab="Total catches")
legend(x=4.5, y=360, rev(c("forest specialists", "forest generalists", "forest visitors", "non forest species","not categorized")), fill=rev(c("gray7","gray27","gray47","gray67","gray87")))
savePlot("stacked barchat birdhab numbers.emf")
x11();par(mar=c(10,4,4,1), xpd=NA, mfrow=c(1,2)) 
barplot(as.matrix(t(sums[,c("FRFRIN","GRGRIN","IN","INGLL","INGR","INSA","NECIN")])), ylim=c(0,300), names.arg=c("Busaju:Bushland","Tengele:Bushland", "Ongo:Farmland","Rwesama:Farmland","Kasokwa:Sugarcane","Kinyara:Sugarcane","RP7:Continuous Forest"), col=c("gray17","gray37","gray57","gray67","gray77","gray87","gray97"), las=2 , ylab="Total catches")
legend(x=8.5, y=360, rev(c("FR & FRIN","GR & GRIN","IN","INGLL","INGR","INSA","NECIN")), fill=rev(c("gray17","gray37","gray57","gray67","gray77","gray87","gray97")))
savePlot("stacked barchat birdfeed numbers.emf")

x11();par(mar=c(10,4,4,1), xpd=NA, mfrow=c(1,2))

hguilds<-as.matrix(forests.guilds.byspp[,c("FF","F","FV","NF")])
hguilds2<-hguilds/rowSums(hguilds)
fguilds<-as.matrix(forests.guilds.byspp[,c("FRFRIN","GRGRIN","IN","INGLL","INGR","INSA","NECIN")])
fguilds2<-fguilds/rowSums(fguilds)

barplot(t(hguilds2), ylim=c(0,1), names.arg=c("Busaju:Bushland","Tengele:Bushland", "Ongo:Farmland","Rwesama:Farmland","Kasokwa:Sugarcane","Kinyara:Sugarcane","RP7:Continuous Forest"), col=c("gray7","gray27","gray47","gray87"), las=2, ylab="Proportion of species",width=rowSums(fguilds))
legend(x=255, y=1, rev(c("forest specialists", "forest generalists", "forest visitors", "non forest species")), fill=rev(c("gray7","gray27","gray47","gray87")))
lines (c(255,265),c(0.5,0.5), lwd=2)
text(330,0.5,"10 species")
text(-70,1.05, "a)")
savePlot("stacked barchat birdhab species.emf", type="emf")

x11();par(mar=c(10,4,4,1), xpd=NA, mfrow=c(1,2)) 
(barplot(t(fguilds2), ylim=c(0,1), names.arg=c("Busaju:Bushland","Tengele:Bushland", "Ongo:Farmland","Rwesama:Farmland","Kasokwa:Sugarcane","Kinyara:Sugarcane","RP7:Continuous Forest"), col=c("gray17","gray37","gray57","gray67","gray77","gray87","gray97"), las=2 , ylab="Proportion of species", width=rowSums(fguilds)))
legend(x=255, y=1, rev(c("Frugivore","Granivore","Insectivore (general)","Gleaning insectivore","Ground insectivore","Sallying insectivore","Nectarivore")), fill=rev(c("gray17","gray37","gray57","gray67","gray77","gray87","gray97")))
lines (c(255,265),c(0.5,0.5), lwd=2)
text(330,0.5,"10 species")
text(-70,1.05, "b)")
savePlot("stacked barchat birdfeed byspp.emf", type="emf")

#massive compound figure without overall values. Has not had arrows added
x11(); par(mar=c(3,3,0,1),oma=c(1,3,3,1), mfrow=c(4,4), xpd=NA, ann=F)
plot(total$totalbirds~total$Distance, ylim=c(0,120), xlim=c(0,15), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$FragmentSize, ylim=c(0,120), xlim=c(0,500), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$CanopyOpeness, ylim=c(0,120), xlim=c(0,40), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$Distance, type="n", ann=FALSE, axes=F, box="n")
legend("topleft",c("Bushland","Farmland","Sugarcane", "Continuous Forest"),pch=21, pt.bg=c("gray27","gray57","gray87","gray7"))

plot(total$spprich~total$Distance, ylim=c(0,30), xlim=c(0,15), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$spprich~total$FragmentSize, ylim=c(0,30), xlim=c(0,500), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$spprich~total$CanopyOpeness, ylim=c(0,30), xlim=c(0,40), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$Distance, type="n", ann=FALSE, axes=F, box="n") 

plot((total$FF/total$totalbirds)~total$Distance,ylim=c(0,1), xlim=c(0,15), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$FF/total$totalbirds)~total$FragmentSize, ylim=c(0,1), xlim=c(0,500), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$FF/total$totalbirds)~total$CanopyOpeness, ylim=c(0,1), xlim=c(0,40), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$Distance, type="n", ann=FALSE, axes=F, box="n")

plot((total$INGR/total$totalbirds)~total$Distance, ylim=c(0,0.4), xlim=c(0,15), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$INGR/total$totalbirds)~total$FragmentSize, ylim=c(0,0.4), xlim=c(0,500), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$INGR/total$totalbirds)~total$CanopyOpeness, ylim=c(0,0.4), xlim=c(0,40), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$Distance, type="n", ann=FALSE, axes=F, box="n") 

mtext("Proportion of                    Proportion of                       Species richness           Abundance       ", side = 2, line = 1, outer = TRUE, cex=0.8)
mtext("ground insectivore indviduals            forest specialist individuals                                                                           ", side = 2, line = 0, outer = TRUE, cex=0.8)

                           
mtext("Distance from forest (km)        Fragment size        Canopy openness (%)                          ", side = 3, line = 1, outer = TRUE, cex=0.8)
  savePlot("compound figs 1.emf", type="emf")
 savePlot("compound figs 1.eps", type="eps")
 

#comparing rich and rare side-by-side
 x11(); par(mar=c(3,3,0,1),oma=c(1,3,3,1), mfrow=c(2,3), ann=F)
errbar(sums$Distance, sums$spprich, maxes$spprich, mins$spprich, ylab="Total number species", ylim=c(0,40), xlim=c(0,15), col= c("gray27", "red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)")
errbar(sums$FragmentSize, sums$spprich, maxes$spprich, mins$spprich, ylab="Total number species", ylim=c(0,40), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Fragment size (ha)")
arrows(460,sums$spprich[sums$Group.1=="RP7"], 500,sums$spprich[sums$Group.1=="RP7"], length=0.08, lwd=2,col="red") 
errbar(sums$V1, sums$spprich, maxes$spprich, mins$spprich,  ylab="Total number species", ylim=c(0,40), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")

errbar(sums$Distance, sums$rarefied, maxes$rarefied, mins$rarefied,  ylab="Rarefied species richness", ylim=c(0,45), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)")
errbar(sums$FragmentSize, sums$rarefied, maxes$rarefied, mins$rarefied,  ylim=c(0,45), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Fragment size (ha)")
arrows(460,sums$rarefied[sums$Group.1=="RP7"], 500,sums$rarefied[sums$Group.1=="RP7"], length=0.08, lwd=2,col="red")
errbar(sums$V1, sums$rarefied, maxes$rarefied, mins$rarefied, ylab="Rarefied species richness", ylim=c(0,45), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Canopy Openness (%)")
 savePlot("choice rare or regular rich.eps", type="eps")


 # Two compound figures with greyscale error bars (first goes abun-rare, second goes FF-INGR. No legend - can do in figure legend text)   CURRENTLY IN MS fig 2
#x11(); par(mar=c(2,5,3,0.1),oma=c(0,0,0,0), mfrow=c(2,3), ann=F, mgp=c(3,0.4,0), tcl=-0.4)
#ostrich - tickmarks inside, axis labels uppercase except units lowecase, box 0.5 points, check tickmarks are 0.5 points 
 
 
  x11(7,5, pointsize=9); par(oma=c(0,0,0,0), mar=c(4,4,0.5,0.5), mfrow=c(2,3), tcl=0.5, mgp=c(2,0.3,0))
errbar(sums$Distance, sums$abun, maxes$totalbirds, mins$totalbirds, ylim=c(0,300), xlim=c(-1,15), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5,errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)], xlab="DISTANCE FROM FOREST (km)", ylab="NUMBER OF BIRDS NETTED", las=1)
text(-0.8,290,"a", adj = c(0,0), cex=1.3, xpd=T)
errbar(sums$FragmentSize, sums$abun, maxes$totalbirds, mins$totalbirds, ylim=c(0,300), xlim=c(-20,500), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)], xlab="FRAGMENT SIZE (ha)", ylab="NUMBER OF BIRDS NETTED", las=1)
arrows(460,sums$abun[sums$Group.1=="RP7"], 500,sums$abun[sums$Group.1=="RP7"], length=0.08, lwd=2,col="black")
text(-15,290,"b", adj = c(0,0), cex=1.3, xpd=T)
errbar(sums$V1, sums$abun, maxes$totalbirds, mins$totalbirds, ylim=c(0,300), xlim=c(-2,25), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, xpd=NA,errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],xlab="CANOPY OPENNESS (%)", ylab="NUMBER OF BIRDS NETTED", las=1)
text(-1.7,290,"c", adj = c(0,0), cex=1.3)
errbar(sums$Distance, sums$rarefied, maxes$rarefied, mins$rarefied,  ylim=c(0,20), xlim=c(-1,15), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],, xlab="DISTANCE FROM FOREST (km)", ylab="RAREFIED SPECIES RICHNESS", las=1)
text(-0.8,19,"d", adj = c(0,0), cex=1.3, xpd=T) 
errbar(sums$FragmentSize, sums$rarefied, maxes$rarefied, mins$rarefied,  ylim=c(0,20), xlim=c(-20,500), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)], xlab="FRAGMENT SIZE (ha)", ylab="RAREFIED SPECIES RICHNESS", las=1)
arrows(460,sums$rarefied[sums$Group.1=="RP7"], 500,sums$rarefied[sums$Group.1=="RP7"], length=0.08, lwd=2,col="black")
text(-15,19,"e", adj = c(0,0), cex=1.3, xpd=T)
errbar(sums$V1, sums$rarefied, maxes$rarefied, mins$rarefied, ylim=c(0,20), xlim=c(-2,25), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15),  cex=1.5, xpd=NA, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)], xlab="CANOPY OPENNESS (%)", ylab="RAREFIED SPECIES RICHNESS", las=1)
text(-1.7,19,"f", adj = c(0,0), cex=1.3, xpd=T)

 savePlot("3x2 abunrich.emf", type="emf")
 savePlot("3x2 abunrich.eps", type="eps")
 savePlot("3x2 abunrich.pdf", type="pdf")

#not in ms now, next one down is 
 x11(); par(mar=c(2,5,3,0.1),oma=c(0,0,0,0), mfrow=c(2,3), ann=F, mgp=c(3,0.4,0), tcl=-0.4)
errbar(sums$Distance,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, ylim=c(0,1), xlim=c(0,15), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)])

text(0,1.1,"Distance from forest (km)", adj = c(0,0), cex=1.3, xpd=T)
text(-4,0.2,"Proportion FF individuals", adj = c(0,0), cex=1.3, xpd=T, srt=90)         
errbar(sums$FragmentSize,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, ylim=c(0,1), xlim=c(0,500), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)])

arrows(460,(sums$FF[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), 500,(sums$FF[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="black")
errbar(sums$V1,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, ylim=c(0,1), xlim=c(0,25), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)])
text(0,1.1,"Canopy openness (%)", adj = c(0,0), cex=1.3, xpd=T)

errbar(sums$Distance,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, ylim=c(0,0.4), xlim=c(0,15), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)])
text(-4,0.1,"Proportion INGR individuals", adj = c(0,0), cex=1.3, xpd=T, srt=90) 
errbar(sums$FragmentSize,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, ylim=c(0,0.4), xlim=c(0,500), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)])
arrows(460,(sums$INGR[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), 500,(sums$INGR[sums$Group.1=="RP7"]/sums$totalbirds[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="black")
errbar(sums$V1,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, ylim=c(0,0.4), xlim=c(0,25), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)])

 savePlot("3x2 FF INGR by individuals.emf", type="emf")
 savePlot("3x2 FF INGR by individuals.eps", type="eps")
 savePlot("3x2 FF INGR by individuals.pdf", type="pdf")

#by prop spp - here and the stacked barplots are the only places I have done this.  CURRENTLY IN MS fig 4
 x11(7,5, pointsize=9 ); par(oma=c(0,0,0,0), mar=c(4,4,0.5,0.5), mfrow=c(2,3), tcl=0.5, mgp=c(2,0.3,0))
coloury= c("gray7","black","gray37","gray67")
errbar(sums$Distance,(forests.guilds.byspp$FF/sums$spprich) , maxes.byspp$FFprop, mins.byspp$FFprop,ylim=c(0,1), xlim=c(-1,15), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=coloury[as.factor(sums$Matrix.type)], xlab="DISTANCE FROM FOREST (km)", ylab="PROPORTION FOREST SPECIALIST SPECIES", las=1)
text(-1,0.95,"a", adj = c(0,0), cex=1.3, xpd=T)
errbar(sums$FragmentSize,(forests.guilds.byspp$FF/sums$spprich) , maxes.byspp$FFprop, mins.byspp$FFprop,ylim=c(0,1), xlim=c(-20,500), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)], xlab="FRAGMENT SIZE (ha)", ylab="PROPORTION FOREST SPECIALIST SPECIES", las=1)
text(-15,0.95,"b", adj = c(0,0), cex=1.3, xpd=T)
arrows(460,(forests.guilds.byspp$FF[forests.guilds.byspp$Group.1=="RP7"]/sums$spprich[sums$Group.1=="RP7"]), 500,(forests.guilds.byspp$FF[forests.guilds.byspp$Group.1=="RP7"]/sums$spprich[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="black")
errbar(sums$V1,(forests.guilds.byspp$FF/sums$spprich) , maxes.byspp$FFprop, mins.byspp$FFprop, ylim=c(0,1), xlim=c(-2,25), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],xlab="CANOPY OPENNESS (%)", ylab="PROPORTION FOREST SPECIALIST SPECIES", las=1)
text(-1.7,0.95,"c", adj = c(0,0), cex=1.3)

errbar(sums$Distance,(forests.guilds.byspp$INGR/sums$spprich) , maxes.byspp$INGRprop, mins.byspp$INGRprop, ylim=c(0,0.4), xlim=c(-1,15), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)], xlab="DISTANCE FROM FOREST (km)", ylab="PROPORTION GROUND-DWELLING INSECTIVORE SPECIES          ", las=1) 
text(-0.8,0.37,"d", adj = c(0,0), cex=1.3, xpd=T) 
errbar(sums$FragmentSize,(forests.guilds.byspp$INGR/sums$spprich) , maxes.byspp$INGRprop, mins.byspp$INGRprop, ylim=c(0,0.4), xlim=c(-20,500), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, errbar.col=coloury[as.factor(sums$Matrix.type)],xlab="FRAGMENT SIZE (ha)", ylab="PROPORTION GROUND-DWELLING INSECTIVORE SPECIES          ", las=1)
text(-15,0.37,"e", adj = c(0,0), cex=1.3, xpd=T)
arrows(460,(forests.guilds.byspp$INGR[forests.guilds.byspp$Group.1=="RP7"]/sums$spprich[sums$Group.1=="RP7"]), 500,(forests.guilds.byspp$INGR[forests.guilds.byspp$Group.1=="RP7"]/sums$spprich[sums$Group.1=="RP7"]), length=0.08, lwd=2,col="black")
errbar(sums$V1,(forests.guilds.byspp$INGR/sums$spprich) , maxes.byspp$INGRprop, mins.byspp$INGRprop, ylim=c(0,0.4), xlim=c(-2,25), col= c("gray7","black","gray37","gray67")[as.factor(sums$Matrix.type)],pch=c(rep(16,6),15), cex=1.5, errbar.col=coloury[as.factor(sums$Matrix.type)],xlab="CANOPY OPENNESS (%)", ylab="PROPORTION GROUND-DWELLING INSECTIVORE SPECIES          ", las=1)
text(-1.7,0.37,"f", adj = c(0,0), cex=1.3, xpd=T)

savePlot("3x2 FF INGR by species.emf", type="emf")
savePlot("3x2 FF INGR by species.eps", type="eps")
savePlot("3x2 FF INGR by species.pdf", type="pdf")