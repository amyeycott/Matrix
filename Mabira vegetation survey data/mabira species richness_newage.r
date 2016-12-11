source("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\matrix veg load data.r")
ls()
#stuff not necessary for ms but possibly useful in future or for exploratory is commented out
# richard made this - it makes a data frame with the species richnesses in, with the different plant functional groups along the top and transect number down the side
rich<-sapply(list(spores=spore.all, seeds=seed.all, trees=trees, trueherbs=trueherbs, ferns=ferns, subcanopywoody=subcanopywoody), function(x){
  rowSums(x>0)
})
rich<-as.data.frame(rich)
rich

# then that data frame is used to make scatterplots of the species richnesses of different groups, and round gives the correlation coefficinet 'r'
#pairs(rich)
#savePlot("pairs.pdf", type="pdf")
#round(cor(rich, use="pair"),2)

#this makes histograms of the frequencies of different species richness values among sampls
#x11();par(mfrow=c(3,2))
#mapply(hist, x=rich, main=names(rich))
#savePlot("rich_hists.pdf", type="pdf")
rich

#new analysis: standardise richness and sample number, then make histograms
rich$standardferns<- rich$ferns/max(rich$ferns, na.rm=T)
rich$standardtrueherbs<- rich$trueherbs/max(rich$trueherbs, na.rm=T)
rich$standardsubcanopywoody<- rich$subcanopywoody/max(rich$subcanopywoody, na.rm=T)
rich$standardtrees<- rich$trees/max(rich$trees, na.rm=T)
 rich
x11();par(mfrow=c(2,2)) 
plot(density(rich$standardtrueherbs, na.rm=T), main="herbs")
#plot(density(rich$standardsubcanopywoody, na.rm=T), main="scw")
#plot(density(rich$standardtrees, na.rm=T), main="trees")
#plot(density(rich$standardferns, na.rm=T), main="(ferns)")

## next step is to make histograms for rich by species group and forest type. First I made a data frame of species richnesses of the 'new' groups (ie combining shrubs and tree seedlings into subcanopywoody). I did a set of scatterplots of just these groups without the seeds or spores

rich2<-sapply(list(trees=trees, trueherbs=trueherbs, ferns=ferns, subcanopywoody=subcanopywoody), function(x){
  rowSums(x>0)
})
rich2<-as.data.frame(rich2)
rich2
pairs(rich2)

# then I added the environment data, to get the forest type ###########Warning MAGIC NUMBER HERE for forest age
envrich <- merge(rich2,env,by.x=0, by.y=0)
head(envrich)
envrich<-envrich[,c(2:5,26)]
str(envrich)

# then I changed the data frame to one with three variables: species richness, plant functional group, and forest type, so that I could make a grouped boxplot
library(reshape)
meltedrich <- melt(envrich, id="newage")
str(meltedrich)
meltedrich<-meltedrich[(!is.na(meltedrich$value)),]
xtabs(value~variable+newage, data=meltedrich)
meltedrich$variable<-factor(meltedrich$variable,levels=c("ferns", "trueherbs", "subcanopywoody", "trees"))

#Then I do the same as for lines 19-43 but for abundances instead of richnesses, to make the second boxplot
abun2<-sapply(list(ferns=fernabun, trueherbs=trueherbs, subcanopywoody=subcanopywoody, trees=trees), rowSums)

abun2<-as.data.frame(abun2)
abun2
envabun <- merge(abun2,env,by.x=0, by.y=0)
head(envabun)
envabun<-envabun[,c(2:5,26)] ####MAGIC NUMBER!!!###
str(envabun)

library(reshape)
meltedabun <- melt(envabun, id="newage")
str(meltedabun)
meltedabun<-meltedabun[(!is.na(meltedabun$value)),]
meltedabun
#these must be bullshit for cover variables...
meltedabun$valuem2<-NA
meltedabun$valuem2[(meltedabun$variable=="ferns")]<-meltedabun$value[(meltedabun$variable=="ferns")]/300#300 m2 is plot area
meltedabun$valuem2[(meltedabun$variable=="trees")]<-meltedabun$value[(meltedabun$variable=="trees")]/300

meltedabun$trans<-sqrt(meltedabun$valuem2)

# here is the code for the grouped boxplot, 'par' just sets the margins to make extra room for making the axis labels sideways. 'At' is the bit that organises the boxplots into groups. 
x11();par(mar=c(3,4,2,1),mfrow=c(2,1), mgp=c(3,0.3,0), tcl=-0.2, bty="n")
boxplot(meltedrich$value~meltedrich$newage*meltedrich$variable, at=(c(15,14,13,11,10,9,7,6,5,3,2,1)), xlim=c(1,15), las=2, xaxt="n", cex.axis=0.75, ylim=c(0,25)) #mess with at to make the ys os og order
axis(side=1, at=(c(15,14,13,11,10,9,7,6,5,3,2,1)), labels=rep(c("OG","MS","YS"),4), cex.axis=0.85, lwd=0)
mtext("Forest type", side=1, line=1.5)
mtext ("Trees > 10cm dbh*        Sub-canopy woody (ns)          Herbs (ns)                     Ferns (ns)", side=3, line=0, cex=0.85, adj=0)
mtext("a", side=3, line=1, adj=0)
mtext("Species richness",side=2,line=2)


boxplot(meltedabun$valuem2[meltedabun$variable=="trees"]~meltedabun$newage[meltedabun$variable=="trees"]*meltedabun$variable[meltedabun$variable=="trees"], at=(c(15,14,13,11,10,9,7,6,5,3,2,1)), xlim=c(1,15), ylim=c(0,0.1), cex.axis=0.75, las=2, xaxt="n")
# this puts the tree abundance data in
boxplot(meltedabun$valuem2[meltedabun$variable=="ferns"]/10~meltedabun$newage[meltedabun$variable=="ferns"]*meltedabun$variable[meltedabun$variable=="ferns"], at=(c(15,14,13,11,10,9,7,6,5,3,2,1)), xlim=c(1,15), ylim=c(0,0.1), cex.axis=0.75, las=2, xaxt="n", add=TRUE)
#then the ferns, scaled to match (div10)
boxplot(meltedabun$value[is.na(meltedabun$valuem2)]/1000~meltedabun$newage[is.na(meltedabun$valuem2)]*meltedabun$variable[is.na(meltedabun$valuem2)], at=(c(15,14,13,11,10,9,7,6,5,3,2,1)), xlim=c(1,15), cex.axis=0.75, las=2, xaxt="n", add=TRUE)
 #this puts the cover data in scaled to fit the plot nicely. It's a dirty fix
axis(side=2, at=c(0,0.02, 0.04,0.06,0.08,0.1), labels=c(0,20,40,60,80,100), las=2, pos=4.5, cex.axis=0.75)
axis(side=2, at=c(0,0.02, 0.04,0.06,0.08,0.1), labels=c(0,20,40,60,80,100), las=2, pos=8.5, cex.axis=0.75)
axis(side=2, at=c(0,0.02, 0.04,0.06,0.08,0.1), labels=c(0,0.2,0.4,0.6,0.8,1.0), las=2, pos=12.5, cex.axis=0.75)

axis(side=1, at=(c(15,14,13,11,10,9,7,6,5,3,2,1)), labels=rep(c("OG","MS","YS"),4), cex.axis=0.85, lwd=0)
mtext("Forest type", side=1, line=1.5)
mtext ("Trees > 10cm dbh (ns)       Sub-canopy woody**          Herbs (ns)                     Ferns (ns)", side=3, line=0, cex=0.85, adj=0)
mtext("b", side=3, line=1, adj=0)
mtext(expression(paste("Total cover or abundance per ",~m^2)),side=2,line=2)


savePlot("Figure 2 grouped rich and abundance boxplots NEW AGE m2.emf", type="emf")
savePlot("Figure 2ab Eycott et al boxplots.eps", type="eps")

savePlot("Figure 2ab Eycott et al boxplots.pdf", type="pdf")

library(car)  #has Levene test

by(meltedrich,meltedrich$variable, function(x)leveneTest(value~newage, data=x))#ns 
by(meltedrich,meltedrich$variable, function(x)summary(aov(value~newage, data=x)))

by(meltedabun,meltedabun$variable, function(x)leveneTest(valuem2~newage, data=x))#m2 only relevant for ferns ns for ferns, trees. sig for cover vars! 
hist(sqrt(meltedabun$valuem2[meltedabun$variable=="trueherbs"]), breaks=10)
hist(sqrt(meltedabun$valuem2[meltedabun$variable=="subcanopywoody"]), breaks=10)


by(meltedabun,meltedabun$variable, function(x)summary(aov(valuem2~newage, data=x)))   #Applies only to ferns and trees. ns for ferns and trees but approaches sig for trees (checked them separately, yes the order is as presumed - ferns, trueherbs, scw, trees)
meltedabun$trans<-sqrt(meltedabun$value)
by(meltedabun,meltedabun$variable, function(x)summary(aov(sqrt(value)~newage, data=x)))  #Applies only to scw and herbs - that is, % cover ones. Sig for scw and also trees...


tapply(meltedabun$valuem2[meltedabun$variable=="trueherbs"], meltedabun$newage[meltedabun$variable=="trueherbs"], mean)
tapply(meltedabun$trans[meltedabun$variable=="subcanopywoody"], meltedabun$newage[meltedabun$variable=="subcanopywoody"], median)
tapply(meltedabun$trans[meltedabun$variable=="subcanopywoody"], meltedabun$newage[meltedabun$variable=="subcanopywoody"], mean)

blank

#colour plots of spprich for herbs, seeds and trees against forest age, not currently in ms
#boxplot(rowSums(trueherbs>0)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Herb Spp rich by Forest age", xlab="Forest Age", ylab="Spprich")
#boxplot(rowSums(seed.all>0)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Seed Spp rich by Forest age", xlab="Forest Age", ylab="Spprich")
#boxplot(rowSums(trees>0)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Tree Spp rich by Forest age", xlab="Forest Age", ylab="Spprich")

#plot herbspprich against env vars, with points coloured by forest age
#boxplot(rowSums(trueherbs>0)~env$Stumps, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Herb Spp rich by n. stumps", xlab="Stumps", ylab="Spprich")
#savePlot("herb rich by stumps.emf")
#boxplot(rowSums(trueherbs>0)~env$m.from.stream, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Herb Spp rich by m.from.stream", xlab="m.from.stream", ylab="Spprich")
#savePlot("herb rich by mfromstream.emf")

#plot (rowSums(trueherbs>0)~env$Slope, col=as.factor(env$newage), xlab="Slope", ylab="n herb species")
#savePlot("herb rich by Slope.emf")
#plot (rowSums(trueherbs>0)~env$BA, col=as.factor(env$newage), xlab="BA", ylab="n herb species")
#savePlot("herb rich by BA.emf")
#plot (rowSums(trueherbs>0)~env$Deadwood, col=as.factor(env$newage), xlab="Deadwood", ylab="n herb species")
#savePlot("herb rich by Deadwood.emf")
#plot (rowSums(trueherbs>0)~env$LLCov, col=as.factor(env$newage), xlab="log(Leaf Litter percent Cover)", ylab="n herb species")
#savePlot("herb rich by LLCov.emf")
#plot (rowSums(trueherbs>0)~env$LLDepth, col=as.factor(env$newage), xlab="LLDepth", ylab="n herb species")
#savePlot("herb rich by LLDepth.emf")
#plot (rowSums(trueherbs>0)~env$CanO, col=as.factor(env$newage), xlab="CanO", ylab="n herb species")
#savePlot("herb rich by CanO.emf")
#plot (rowSums(trueherbs>0)~env$pH, col=as.factor(env$newage), xlab="Soil pH", ylab="n herb species")
#savePlot("herb rich by soil pH.emf")
#plot (rowSums(trueherbs>0)~env$N., col=as.factor(env$newage), xlab="Soil nitrogen", ylab="n herb species")
#savePlot("herb rich by soil Nitrogen.emf")
#plot (rowSums(trueherbs>0)~env$C., col=as.factor(env$newage), xlab="Soil carbon", ylab="n herb species")
#savePlot("herb rich by soil carbon.emf")
#plot (rowSums(trueherbs>0)~env$TCat, col=as.factor(env$newage), xlab="total cations", ylab="n herb species")
#savePlot("herb rich by soil total cations.emf")
#add a legend. Note that R will wait for you to click the bit of the graph where you want the legend>
#legend(locator(1),c("og","os","ys"),pch=c(1,1,1),col=c(1,2,3))

#plot cover/total seeds for herbs, seeds and trees against forest age
#boxplot(rowSums(trueherbs)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main="Ground veg angiosperm cover by Forest age", xlab="Forest Age", ylab="Total cover")
#boxplot(rowSums(seed.all)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Seed Spp rich by Forest age", xlab="Forest Age", ylab="Total herb cover")
#boxplot(rowSums(trees)~env$newage, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Tree Spp rich by Forest age", xlab="Forest Age", ylab="Total herb cover")


#plot herb cover against env vars, with points coloured by forest age
#boxplot(rowSums(trueherbs)~env$Stumps, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Herb Spp rich by n. stumps", xlab="Stumps", ylab="Spprich")
#savePlot("herb rich by stumps.emf")
#boxplot(rowSums(trueherbs)~env$m.from.stream, col=(c("gold","darkgreen","red")), notch=TRUE, main=" Herb Spp rich by m.from.stream", xlab="m.from.stream", ylab="Spprich")
#savePlot("herb rich by mfromstream.emf")
#plot (rowSums(trueherbs)~env$Slope, col=as.factor(env$newage), xlab="Slope", ylab="Total herb cover")
#savePlot("herb rich by Slope.emf")
#plot (rowSums(trueherbs)~env$BA, col=as.factor(env$newage), xlab="BA", ylab="Total herb cover")
#savePlot("herb rich by BA.emf")
#plot (rowSums(trueherbs)~env$Deadwood, col=as.factor(env$newage), xlab="Deadwood", ylab="Total herb cover")
#savePlot("herb rich by Deadwood.emf")
#plot (rowSums(trueherbs)~env$LLCov, col=as.factor(env$newage), xlab="LLCov", ylab="Total herb cover")
#savePlot("herb rich by LLCov.emf")
#plot (rowSums(trueherbs)~env$LLDepth, col=as.factor(env$newage), xlab="LLDepth", ylab="Total herb cover")
#savePlot("herb rich by LLDepth.emf")
#plot (rowSums(trueherbs)~env$CanO, col=as.factor(env$newage), xlab="CanO", ylab="Total herb cover")
#savePlot("herb rich by CanO.emf")
#plot (rowSums(trueherbs)~env$pH, col=as.factor(env$newage), xlab="Soil pH", ylab="Total herb cover")
#savePlot("herb rich by soil pH.emf")
#plot (rowSums(trueherbs)~env$N., col=as.factor(env$newage), xlab="Soil nitrogen", ylab="Total herb cover")
#savePlot("herb rich by soil Nitrogen.emf")
#plot (rowSums(trueherbs)~env$C., col=as.factor(env$newage), xlab="Soil carbon", ylab="Total herb cover")
#savePlot("herb rich by soil carbon.emf")
#plot (rowSums(trueherbs)~env$TCat, col=as.factor(env$newage), xlab="total cations", ylab="Total herb cover")
#savePlot("herb rich by soil total cations.emf")


##SPECIES ACCUMULATION CURVES## 
#species accumulation curves by functional groups. the species accumulation function hates NA values, so I have to tell it to use data only for those plots where we know the species richness and the age. 
ls()
levels(env$newage)

keep2c<-!is.na(trueherbs[,1])
keep3c<-!is.na(ferns[,1])
keep5c<-!is.na(trees[,1])
keep6c<-!is.na(subcanopywoody[,1])
keep2c

spaccherb <- specaccum(trueherbs[keep2c,], "exact")
spaccfern <- specaccum(ferns[keep3c,], "exact")
spaccbigtrees <- specaccum(trees[keep5c,], "exact")
spaccsubcanopywoody <- specaccum(subcanopywoody[keep6c,], "exact")                      

plot(spaccfern, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(1,70), xaxs="i", ylim=c(0,100), yaxs="i", bty="l")
plot(spaccherb, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(spaccbigtrees, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
plot(spaccsubcanopywoody, add=T, ci.type="line", col="gold", lwd=2, ci.lty=1, ci.col="palegoldenrod")
legend("topright",c("Ferns","Herbs","Sub-canopy woody", "Trees >10cm diameter"),pch=16,col=c("blue","green","gold", "red"))
savePlot("species accumulation curves_bright colours.emf", type="emf")

## species accumulation curves, by forest age.
x11();par(mfrow=c(2,2))
sa_fernog <- specaccum(ferns[keep3c&env$newage=="og",], "exact")
sa_fernos <- specaccum(ferns[keep3c&env$newage=="os",], "exact")
sa_fernys <- specaccum(ferns[keep3c&env$newage=="ys",], "exact")
plot(sa_fernos, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(0,25), ylim=c(0,50), yaxs="i", main="Ferns", bty="l", xaxs="i")
plot(sa_fernys, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(sa_fernog, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
legend("topleft",c("og","os","ys"),pch=16,col=c("red","blue","green"))
savePlot("species accumulation curves_ferns_bright colours.emf")

sa_herbog <- specaccum(trueherbs[keep2c&env$newage=="og",], "exact")
sa_herbos <- specaccum(trueherbs[keep2c&env$newage=="os",], "exact")
sa_herbys <- specaccum(trueherbs[keep2c&env$newage=="ys",], "exact")
plot(sa_herbos, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(0,25), ylim=c(0,50), yaxs="i", main="Herbs", bty="l", xaxs="i")
plot(sa_herbys, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(sa_herbog, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
legend("topleft",c("og","os","ys"),pch=16,col=c("red","blue","green"))
savePlot("species accumulation curves_herbs_bright colours.emf")

sa_scw_og <- specaccum(subcanopywoody[keep6c&env$newage=="og",], "exact")
sa_scw_os <- specaccum(subcanopywoody[keep6c&env$newage=="os",], "exact")
sa_scw_ys <- specaccum(subcanopywoody[keep6c&env$newage=="ys",], "exact")
plot(sa_scw_os, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(0,25), ylim=c(0,50), yaxs="i", main="Sub-canopy woody plants", bty="l", xaxs="i")
plot(sa_scw_ys, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(sa_scw_og, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
legend("topleft",c("og","os","ys"),pch=16,col=c("red","blue","green"))
savePlot("species accumulation curves_scw_bright colours.emf")

sa_trees_og <- specaccum(trees[keep5c&env$newage=="og",], "exact")
sa_trees_os <- specaccum(trees[keep5c&env$newage=="os",], "exact")
sa_trees_ys <- specaccum(trees[keep5c&env$newage=="ys",], "exact")
plot(sa_trees_os, ci.type="line", col="blue", lwd=2, ci.lty=1, ci.col="lightblue", xlim=c(0,25), ylim=c(0,50), yaxs="i", main="Trees", bty="l", xaxs="i")
plot(sa_trees_ys, add=T, ci.type="line", col="green", lwd=2, ci.lty=1, ci.col="lightgreen")
plot(sa_trees_og, add=T, ci.type="line", col="red", lwd=2, ci.lty=1, ci.col="pink")
legend("topright",c("og","os","ys"),pch=16,col=c("red","blue","green"))
savePlot("species accumulation curves_fourplots_bright colours.emf", type="emf")


###VENN DIAGRAMS##
#area1 The size of the first set  ALL OG   
#area2 The size of the second set ALL OS
#area3 The size of the third set  ALL YS
#n12 The size of the intersection between the first and the second set OG & OS
#n23 The size of the intersection between the second and the third set OS & YS
#n13 The size of the intersection between the first and the third set  OG & YS
#n123 The size of the intersection between all three sets
#category A vector (length 3) of strings giving the category names of the sets
#I've tried to eulerise them but it's not happening so I guess it doesn't meet the requirements
library(VennDiagram)
x11();
draw.triple.venn(
area1 = length(which(colSums(trees[keep5c&(env$newage=="og"),])>0)),
area2 = length(which(colSums(trees[keep5c&(env$newage=="os"),])>0)),
area3 = length(which(colSums(trees[keep5c&(env$newage=="ys"),])>0)),
n12 = length(intersect (names(which(colSums(trees[keep5c&(env$newage=="og"),])>0)),names(which(colSums(trees[keep5c&(env$newage=="os"),])>0)))),
n23 = length(intersect (names(which(colSums(trees[keep5c&(env$newage=="os"),])>0)),names(which(colSums(trees[keep5c&(env$newage=="ys"),])>0)))),
n13 = length(intersect (names(which(colSums(trees[keep5c&(env$newage=="og"),])>0)),names(which(colSums(trees[keep5c&(env$newage=="ys"),])>0)))),
n123 =length(intersect(names(which(colSums(trees[keep5c&(env$newage=="og"),])>0)),(intersect(names(which(colSums(trees[keep5c&(env$newage=="os"),])>0)),names(which(colSums(trees[keep5c&(env$newage=="ys"),])>0)))))) ,
category = c("OG", "OS", "YS"),
cex=2,
fontfamily="sans",
cat.cex=2,
cat.fontfamily="sans"
);
savePlot("Venntree.emf", type="emf")
x11();
draw.triple.venn(
area1 = length(which(colSums(trueherbs[keep2c&(env$newage=="og"),])>0)),
area2 = length(which(colSums(trueherbs[keep2c&(env$newage=="os"),])>0)),
area3 = length(which(colSums(trueherbs[keep2c&(env$newage=="ys"),])>0)),
n12 = length(intersect (names(which(colSums(trueherbs[keep2c&(env$newage=="og"),])>0)),names(which(colSums(trueherbs[keep2c&(env$newage=="os"),])>0)))),
n23 = length(intersect (names(which(colSums(trueherbs[keep2c&(env$newage=="os"),])>0)),names(which(colSums(trueherbs[keep2c&(env$newage=="ys"),])>0)))),
n13 = length(intersect (names(which(colSums(trueherbs[keep2c&(env$newage=="og"),])>0)),names(which(colSums(trueherbs[keep2c&(env$newage=="ys"),])>0)))),
n123 =length(intersect(names(which(colSums(trueherbs[keep2c&(env$newage=="og"),])>0)),(intersect(names(which(colSums(trueherbs[keep2c&(env$newage=="os"),])>0)),names(which(colSums(trueherbs[keep2c&(env$newage=="ys"),])>0)))))) ,
category = c("OG", "OS", "YS"),
cex=2,
fontfamily="sans",
cat.cex=2,
cat.fontfamily="sans"
);
savePlot("Venntrueherb.emf", type="emf")         
x11();
draw.triple.venn(
area1 = length(which(colSums(ferns[keep3c&(env$newage=="og"),])>0)),
area2 = length(which(colSums(ferns[keep3c&(env$newage=="os"),])>0)),
area3 = length(which(colSums(ferns[keep3c&(env$newage=="ys"),])>0)),
n12 = length(intersect (names(which(colSums(ferns[keep3c&(env$newage=="og"),])>0)),names(which(colSums(ferns[keep3c&(env$newage=="os"),])>0)))),
n23 = length(intersect (names(which(colSums(ferns[keep3c&(env$newage=="os"),])>0)),names(which(colSums(ferns[keep3c&(env$newage=="ys"),])>0)))),
n13 = length(intersect (names(which(colSums(ferns[keep3c&(env$newage=="og"),])>0)),names(which(colSums(ferns[keep3c&(env$newage=="ys"),])>0)))),
n123 =length(intersect(names(which(colSums(ferns[keep3c&(env$newage=="og"),])>0)),(intersect(names(which(colSums(ferns[keep3c&(env$newage=="os"),])>0)),names(which(colSums(ferns[keep3c&(env$newage=="ys"),])>0)))))) ,
category = c("OG", "OS", "YS"),
cex=2,
fontfamily="sans",
cat.cex=2,
cat.fontfamily="sans"
);
savePlot("Vennfern.emf", type="emf")
x11();
draw.triple.venn(
area1 = length(which(colSums(subcanopywoody[keep6c&(env$newage=="og"),])>0)),
area2 = length(which(colSums(subcanopywoody[keep6c&(env$newage=="os"),])>0)),
area3 = length(which(colSums(subcanopywoody[keep6c&(env$newage=="ys"),])>0)),
n12 = length(intersect (names(which(colSums(subcanopywoody[keep6c&(env$newage=="og"),])>0)),names(which(colSums(subcanopywoody[keep6c&(env$newage=="os"),])>0)))),
n23 = length(intersect (names(which(colSums(subcanopywoody[keep6c&(env$newage=="os"),])>0)),names(which(colSums(subcanopywoody[keep6c&(env$newage=="ys"),])>0)))),
n13 = length(intersect (names(which(colSums(subcanopywoody[keep6c&(env$newage=="og"),])>0)),names(which(colSums(subcanopywoody[keep6c&(env$newage=="ys"),])>0)))),
n123 =length(intersect(names(which(colSums(subcanopywoody[keep6c&(env$newage=="og"),])>0)),(intersect(names(which(colSums(subcanopywoody[keep6c&(env$newage=="os"),])>0)),names(which(colSums(subcanopywoody[keep6c&(env$newage=="ys"),])>0)))))) ,
category = c("OG", "OS", "YS"),
cex=2,
fontfamily="sans",
cat.cex=2,
cat.fontfamily="sans"
);
savePlot("Vennsubcanopywoody.emf", type="emf")
                
#older versions:
#nicked from data loading file:
#bothsetdiff<-function(x,y)list(unique.to.x=setdiff(x,y),unique.to.y=setdiff(y,x), in.common=intersect(x,y))

#trying to do it somehow: prep these lists then open them in notepad, remove the first row, and resave into a folder just for these files
#oglist<-list(colnames(treesog[,colSums(treesog)>0]))
#oslist<-list(colnames(treesos[,colSums(treesos)>0]))
#yslist<-list(colnames(treesys[,colSums(treesys)>0]))
#write.table(oglist,"ogtrees.txt")
#write.table(oslist,"ostrees.txt")
#write.table(yslist,"ystrees.txt")
#write.table(oglist,"ogtrees.txt", sep="\t")

# this next bit seems to miss the last few off the list
#write.table((rbind(oglist,oslist,yslist)), "eVenn.txt", sep="\t", quote=F)
#write.table(trees,"trees.txt", sep="\t", quote=F)
#write.table(ferns,"ferns.txt", sep="\t", quote=F)
#write.table(subcanopywoody,"subcanopywoody.txt", sep="\t", quote=F)
#write.table(trueherbs,"herbs.txt", sep="\t", quote=F)

####start of making matching sets?
#treeslist<-lapply(levels(env$Forest), function(n){
#   z<-trees[env$Forest==n,]
 #  colnames(z[,colSums(z)>0])
#})

##or the long way
#treesog<-trees[keep5c&(env$Forest=="og"),]
#treesos<-trees[keep5c&(env$Forest=="os"),]
#treesys<-trees[keep5c&(env$Forest=="ys"),]
#str(treesog)
#scwog<-subcanopywoody[keep6c&(env$Forest=="og"),]
#scwos<-subcanopywoody[keep6c&(env$Forest=="os"),]
#scwys<-subcanopywoody[keep6c&(env$Forest=="ys"),]

#this is no use because it misses out the ones in the middle of the venn
#bothsetdiff(colnames(treesog[,colSums(treesog)>0]),colnames(treesos[,colSums(treesos)>0]))
#bothsetdiff(colnames(treesog[,colSums(treesog)>0]),colnames(treesys[,colSums(treesys)>0]))
#bothsetdiff(colnames(treesys[,colSums(treesys)>0]),colnames(treesos[,colSums(treesos)>0]))


#looking at compartments to try to fix age class mystery
coloury<-unique(cbind(env$newage,env$Compartment))
coloury<-coloury[order(coloury[,2]),]
boxplot(Stumps~Compartment, data=env, col=coloury[,1]) 
boxplot(CanO~Compartment, data=env, col=coloury[,1]) 
boxplot(BA~Compartment, data=env, col=coloury[,1])

