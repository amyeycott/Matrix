# lines with a "#" at the beginning are comments and will not affect the code
#load libraries
library(RODBC)
library(vegan)
#load data, env first then allometry
setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Patrick")
con<-odbcConnectExcel("MATRIX Patrick_env data_Amy file.xls")
sqlTables(con)
transects<-sqlQuery(con,"Select * From [Transect level$]")
quadrats<-sqlQuery(con,"Select * From [Quadrat level$]")
photos<-sqlQuery(con,"Select * From [subsets_photos$]")
close(con)
con2<-odbcConnectExcel("MATRIX Patrick_species data_Amy file.xls")
sqlTables(con2)
thinish<-sqlQuery(con2,"Select * From [Sizedata$]")
herbariumlist<-sqlQuery(con2,"Select * From [Quadrat level$]")
close(con2)
# check it has gone in ok by looking at the structure.
str(transects)
str(quadrats)
str(photos)
str(thinish)

#species data first
thinish$plotid<-as.factor(paste(thinish$Forest, "_", thinish$Transect))
thinish$Abundance<-as.numeric(1) #but this is broken so it's just presence-absence for now. There is abundance data.
ferns<-as.data.frame(unclass(xtabs(Abundance~plotid+Species, data=thinish[!thinish$Species=="NoData",])))

##right now mpanga has no data (rows55:84). 27 is nodata and 41 is unidentified
ferns<-ferns[c(1:54,85:111),c(1:26,28:40)]


#then environment data
transects$plotid<-as.factor(paste(transects$Forest, "_", transects$Transect))
quadrats$plotid<-as.factor(paste(quadrats$FOREST, "_", quadrats$Transect))
aggquad<-aggregate(quadrats[4:6], list(quadrats$plotid), mean)
match(levels(quadrats$plotid), levels(aggquad$Group.1))
transects<-merge(transects, aggquad, by.x="plotid", by.y="Group.1")

#making transects order and ferns order match so that forest can be plotted and cca run
transects<-transects[order(transects$plotid),]
rownames(ferns)
transects$plotid
#removing a plot with no ferns and Mpanga which is not ready
slimtransects<-transects[!transects$plotid=="Kisubi-Kabaale _ 1"&!transects$Forest=="Mpanga",]
#removing all the lines with missing environment data, leaves 96 transects
keepT<-!is.na(rowSums(slimtransects[15:20]))&!slimtransects$Disturbance_level=="NoData"&!slimtransects$SLOPE=="NoData"&!is.na(slimtransects$DEADWOOD)&!is.na(slimtransects$STUMPS)




roughlook<-decorana(ferns[rowSums(ferns)>0,])
 plot(roughlook) #is kind of ok as DCA
 summary(roughlook)

roughcca<-cca(ferns[keepT&rowSums(ferns)>0,]~SLOPE+Disturbance_level+av_prism_tally+DEADWOOD+STUMPS+Leaflitter_percent+Leaflitter_cm+Shrubs_percent, data= slimtransects[keepT&rowSums(ferns)>0,])
testcca<-cca(ferns[keepT&rowSums(ferns[,1:47])>0,]~Disturbance_level+av_prism_tally+DEADWOOD+Leaflitter_percent+Shrubs_percent, data= slimtransects[keepT&rowSums(ferns[,1:47])>0,])

test2cca<-cca(ferns[keepT&rowSums(ferns[,1:47])>0,]~Disturbance_level+DEADWOOD, data= slimtransects[keepT&rowSums(ferns[,1:47])>0,])

plot(roughcca)



boxplot(rowSums(ferns>0)~slimtransects$Disturbance_level)
ferns[rowSums(ferns>0)>5,]
hist(rowSums(ferns>0))