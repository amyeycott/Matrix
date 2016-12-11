source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Budongo\\moreen fragments load data.r")

#Here are the spotplots in threes with range bars for ind transects
x11(); par(mar=c(5,4,1,1), mfrow=c(2,2), xpd=NA)

x11(); par(mar=c(5,4,2,1), mfrow=c(2,2), xpd=NA)
plot((sums$FF/sums$totalbirds)~sums$Distance, ylab="Proportion of forest specialists (FF)", ylim=c(0,1), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Distance from forest (km)")
errbar(sums$Distance,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, add=T, type="n")
plot((sums$FF/sums$totalbirds)~sums$FragmentSize, ylab="Proportion of forest specialists (FF)", ylim=c(0,1), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=F, xlab="Fragment size (ha)")
errbar(sums$FragmentSize,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, add=T, type="n")
plot((sums$FF/sums$totalbirds)~sums$V1.y, ylab="Proportion of forest specialists (FF)", ylim=c(0,1), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")
errbar(sums$V1.y,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, add=T, type="n")
legend(x=38, y=0.9,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots FFprop ranges  byspp.eps", type="eps")

x11(); par(mar=c(5,4,1,1), mfrow=c(2,2), xpd=NA)
plot((sums$INGR/sums$totalbirds)~sums$Distance, ylab="Proportion of ground insectivores", ylim=c(0,0.4), xlim=c(0,15), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)")
errbar(sums$Distance,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, add=T, type="n")
plot((sums$INGR/sums$totalbirds)~sums$FragmentSize, ylab="Proportion of ground insectivores", ylim=c(0,0.4), xlim=c(0,500), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=F, xlab="Fragment size (ha)")
errbar(sums$FragmentSize,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, add=T, type="n")
plot((sums$INGR/sums$totalbirds)~sums$V1.y, ylab="Proportion of ground insectivores", ylim=c(0,0.4), xlim=c(0,25), col= c("gray27","red","gray57","gray87")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xpd=NA, xlab="Canopy Openness (%)")
errbar(sums$V1.y,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, add=T, type="n")
legend(x=38, y=0.4,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red"))
savePlot("threespotplots INGRprop ranges byspp.eps", type="eps")

#gleaning insectivores: most species. Note switch to more elegant code
x11(); par(mar=c(5,4,2,1), mfrow=c(2,2), xpd=NA)
errbar(sums$Distance,(sums$INGLL/sums$totalbirds) , maxes$INGLLprop, mins$INGLLprop, ylim=c(0,0.4), xlim=c(0,15), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of gleaning insectivores", xlab="Distance from forest (km)")
errbar(sums$FragmentSize,(sums$INGLL/sums$totalbirds) , maxes$INGLLprop, mins$INGLLprop, ylim=c(0,0.4), xlim=c(0,500), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of gleaning insectivores", xlab="Fragment size (ha)")
errbar(sums$V1.y,(sums$INGLL/sums$totalbirds) , maxes$INGLLprop, mins$INGLLprop, ylim=c(0,0.4), xlim=c(0,25), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of gleaning insectivores", xlab="Canopy Openness (%)")
legend(x=40, y=0.4,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red")) 
savePlot("threespotplots INGLL proportion ranges byspp.emf")

#frugivores - most tested by other people
x11(); par(mar=c(5,4,2,1), mfrow=c(2,2), xpd=NA)
errbar(sums$Distance,(sums$FR/sums$totalbirds) , maxes$FRprop, mins$FRprop, ylim=c(0,0.5), xlim=c(0,15), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of frugivores", xlab="Distance from forest (km)")
errbar(sums$FragmentSize,(sums$FR/sums$totalbirds) , maxes$FRprop, mins$FRprop, ylim=c(0,0.5), xlim=c(0,500), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of frugivores", xlab="Fragment size (ha)")
errbar(sums$V1.y,(sums$FR/sums$totalbirds) , maxes$FRprop, mins$FRprop, ylim=c(0,0.5), xlim=c(0,25), pch=16, col=c("gray27","red","gray57", "gray87")[as.factor(sums$Matrix.type)],cex=1.5,ylab="Proportion of frugivores", xlab="Canopy Openness (%)")
legend(x=40, y=0.4,c("Bushland fragments","Farmland fragments","Sugarcane fragments", "Continuous Forest"),pch=16, col=c("gray27","gray57","gray87","red")) 
savePlot("threespotplots FR proportion ranges byspp.emf")

#stacked barplots of guilds

x11();par(mar=c(10,4,4,1), xpd=T, mfrow=c(1,2))   
barplot(as.matrix(t(sums[,c("FF","F","FV","NF","O")])), ylim=c(0,300), names.arg=c("Busaju:Bushland","Tengele:Bushland", "Ongo:Farmland","Rwesama:Farmland","Kasokwa:Sugarcane","Kinyara:Sugarcane","RP7:Continuous Forest"), col=c("gray7","gray27","gray47","gray67","gray87"), las=2, ylab="Total catches")
legend(x=4.5, y=360, c("forest specialists", "forest generalists", "forest visitors", "non forest species","not categorized"), fill=c("gray7","gray27","gray47","gray67","gray87"))
savePlot("stacked barchat birdhab byspp.emf")

barplot(as.matrix(t(sums[,c("FRFRIN","GRGRIN","IN","INGLL","INGR","INSA","NECIN")])), ylim=c(0,300), names.arg=c("Busaju:Bushland","Tengele:Bushland", "Ongo:Farmland","Rwesama:Farmland","Kasokwa:Sugarcane","Kinyara:Sugarcane","RP7:Continuous Forest"), col=c("gray17","gray37","gray57","gray67","gray77","gray87","gray97"), las=2 , ylab="Total catches")
legend(x=4.5, y=360, c("FR & FRIN","GR & GRIN","IN","INGLL","INGR","INSA","NECIN"), fill=c("gray17","gray37","gray57","gray67","gray77","gray87","gray97"))
savePlot("stacked barchat birdfeed byspp.emf")



plot((total$FF/total$totalbirds)~total$Distance,ylim=c(0,1), xlim=c(0,15), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$FF/total$totalbirds)~total$FragmentSize, ylim=c(0,1), xlim=c(0,500), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$FF/total$totalbirds)~total$CanopyOpeness, ylim=c(0,1), xlim=c(0,40), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$Distance, type="n", ann=FALSE, axes=F, box="n")

plot((total$INGR/total$totalbirds)~total$Distance, ylim=c(0,0.4), xlim=c(0,15), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$INGR/total$totalbirds)~total$FragmentSize, ylim=c(0,0.4), xlim=c(0,500), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot((total$INGR/total$totalbirds)~total$CanopyOpeness, ylim=c(0,0.4), xlim=c(0,40), col= c("gray27","gray57","gray87", "gray7")[as.factor(total$Matrix.type)],pch=16, cex=1.5)
plot(total$totalbirds~total$Distance, type="n", ann=FALSE, axes=F, box="n") 

mtext("Proportion of                    Proportion of                       Species richness           Abundance       ", side = 2, line = 1, outer = TRUE, cex=0.8)
mtext("ground insectivores            forest species                                                                           ", side = 2, line = 0, outer = TRUE, cex=0.8)

                           
mtext("Distance from forest (km)        Fragment size        Canopy openness (%)                          ", side = 3, line = 1, outer = TRUE, cex=0.8)
  savePlot("vvv.emf", type="emf")

 # massive compound fig FF-INGRR. No legend - can do in figure legend text)
#half finished must change order of pics not mfrow. all abun, then all rich, then all div

  x11(); par(mar=c(3,3,0,1),oma=c(1,3,3,1), mfrow=c(2,3), ann=F, pin=c(1.5,1.5))
errbar(sums$Distance,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, ylab="Proportion of forest specialists (FF)", ylim=c(0,1), xlim=c(0,15), col= c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)", errbar.col=c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)])
errbar(sums$FragmentSize,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, ylab="Proportion of forest specialists (FF)", ylim=c(0,1), xlim=c(0,500), col= c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Fragment size (ha)", errbar.col=c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)])
errbar(sums$V1,(sums$FF/sums$totalbirds) , maxes$FFprop, mins$FFprop, ylab="Proportion of forest specialists (FF)", ylim=c(0,1), xlim=c(0,25), col= c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Canopy Openness (%)", errbar.col=c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)])

errbar(sums$Distance,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, ylab="Proportion of ground insectivores", ylim=c(0,0.4), xlim=c(0,15), col= c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Distance from forest (km)", errbar.col=c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)])
errbar(sums$FragmentSize,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, ylab="Proportion of ground insectivores", ylim=c(0,0.4), xlim=c(0,500), col= c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Fragment size (ha)", errbar.col=c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)])
errbar(sums$V1,(sums$INGR/sums$totalbirds) , maxes$INGRprop, mins$INGRprop, ylab="Proportion of ground insectivores", ylim=c(0,0.4), xlim=c(0,25), col= c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)],pch=16, cex=1.5, xlab="Canopy Openness (%)", errbar.col=c("gray7","red","gray37","gray67")[as.factor(sums$Matrix.type)])
mtext("                   Distance from forest (km)        Fragment size (ha)       Canopy openness (%)                          ", side = 3, line = 2, outer = TRUE, cex=0.8)
mtext("                  Proportion INGR                  Proportion FF", side = 2, line = 1, outer = TRUE, cex=0.8)
 savePlot("3x2 FF INGR byspp.eps", type="eps")