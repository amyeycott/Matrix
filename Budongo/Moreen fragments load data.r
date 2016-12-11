library(vegan)
library(Hmisc)
setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Budongo") 
#load data
sppthin<-read.csv( "Mist netting data1.csv", row.names=1)
head(sppthin)           
str(sppthin)
envvars<-read.csv( "all environmental variables.csv", row.names=1)
envvars<-t(envvars)
envvars<-envvars[,1:3]
loc<-sppthin[2:4]
loc<-unique(loc)
rownames(loc)<-paste(loc$Fragment.name,loc$Transect, sep=".")
env<-merge(loc, envvars,by.x=0, by.y=0, sort=FALSE)
row.names(env)<-env$Row.names
#number of individuals of each species in each transect   
spp<-xtabs(No..of.Birds~paste(Fragment.name,Transect, sep=".")+Botanical.name, data=sppthin)
spp<-as.data.frame(unclass(spp))
str(spp)
#number of individuals of each species in each transect in each day      
daily<-xtabs(No..of.Birds~paste(Fragment.name,Transect,Date, sep=".")+Botanical.name, data=sppthin)
daily<-as.data.frame(unclass(daily))
str(daily)
#checking that there are several dates for each transect
daycheck<-xtabs(No..of.Birds~paste(Fragment.name,Transect, sep=".")+Date, data=sppthin)
daycheck<-rowSums((as.data.frame(unclass(daycheck)))>0)
str(daycheck)

#new guild data feb 2014
sppthin$Feeding.guild[sppthin$Botanical.name=="Amandava subflava"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Estrilda nonnula"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Halcyon senegalensis"]<-"IN"
sppthin$Feeding.guild[sppthin$Botanical.name=="Hippolais languida"]<-"IN"     
sppthin$Feeding.guild[sppthin$Botanical.name=="Lagonosticta rubricata"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Lagonosticta senegala"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Lonchura bicolor"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Lonchura cucullata"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Muscicapa caerulescens"]<-"IN"
sppthin$Feeding.guild[sppthin$Botanical.name=="Nesocharis capistrata"]<-"GR"
sppthin$Feeding.guild[sppthin$Botanical.name=="Phylloscopus trochilus"]<-"IN"
sppthin$Feeding.guild[sppthin$Botanical.name=="Ploceus nigricollis"]<-"GRIN"
sppthin$Feeding.guild[sppthin$Botanical.name=="Pycnonotus barbatus"]<-"FRIN" 
sppthin$Feeding.guild<-as.factor(sppthin$Feeding.guild[drop = TRUE])
#combining NF and O habitat guilds
levels(sppthin$Forest.specialisation)
sppthin$Forest.specialisation[sppthin$Forest.specialisation=="O"]<-"NF" 
sppthin$Forest.specialisation<-as.factor(sppthin$Forest.specialisation[drop = TRUE])




#number of birds of each specialisation in each transect
sppspec<-xtabs(No..of.Birds~paste(Fragment.name,Transect, sep=".")+Forest.specialisation, data=sppthin)
sppspec<-as.data.frame(unclass(sppspec))
head(sppspec)
colSums(sppspec)
sppfeed<-xtabs(No..of.Birds~paste(Fragment.name,Transect, sep=".")+Feeding.guild, data=sppthin)
sppfeed<-as.data.frame(unclass(sppfeed))
head(sppfeed)                           
colSums(sppfeed)
total <- merge(env,sppspec, by="row.names")
total<-merge(total, sppfeed, by.x=1, by.y=0)
spprich<-as.data.frame(rowSums(spp>0))
names(spprich)<-"spprich"
total<-merge(total, spprich, by.x=1, by.y=0)
total$totalbirds<-rowSums(total[9:13])
rare<-as.data.frame(rarefy(spp,(min(rowSums(spp)))))
names(rare)<-"rarefied"
total<-merge(total, rare, by.x=1, by.y=0)
row.names(total)<-total$Row.names
total<-total[,3:24]#just tsakes away duplicate rownames cols
sums<-as.data.frame(aggregate(total[7:20], by=list(total$Fragment.name), FUN=sum, na.rm=T))#7:20 just takes the specialisms and spprich
row.names(sums)<-sums$Group.1
sums$totalbirds<-rowSums(sums[,2:6])
temp<-by(spp>0,rep(1:7, each=3), FUN=function(x)sum(colSums(x)>0))
sums$spprich<- sapply(temp, I)
temp
sppagg<-by(spp,rep(1:7, each=3), FUN=function(x)colSums(x))
sppagg<- t(as.data.frame(sapply(sppagg, I)))
rownames(sppagg)<-c("Busaju", "Kasokwa", "Kinyara", "Ongo", "RP7", "Rwesama", "Tengele")
aggrare<-as.data.frame(rarefy(sppagg,(min(rowSums(spp)))))
aggrare$special<-as.vector(rarefy(sppagg,(min(rowSums(spp)))))

trans.HShann<-diversity(spp)
total<-merge(total, trans.HShann, by=0)

names(aggrare)<-c("rarefied", "special")
sums<-merge(sums, aggrare,by=0)
envbyforest<-env[env$Transect==1,c(2:3,6:7)]
sums<-merge(sums, envbyforest, by.x=2, by.y=2)
total$Matrix.type<-factor(total$Matrix.type, levels=c("Bushland",  "Farmland", "Sugarcane","Cont Forest"))#reorder levels of Matrix  
total$Fragment.name<-factor(total$Fragment.name, levels=unique(total$Fragment.name[order(total$Matrix.type)]))#reorder levels of fragmentname according to the values of matrixtype
sums$Group.1<-factor(sums$Group.1, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))#reorder to match total
sumscanopy<-(as.numeric(by(total$CanopyOpeness, total$Fragment.name, FUN=mean, simplify=T)))
sumscanopy<-as.data.frame(t(sumscanopy))
sumscanopy<-t(sumscanopy)
row.names(sumscanopy)<-sums$Group.1
sums<-merge(sums, sumscanopy, by.x=1, by.y=0)
sums$FRFRIN<- sums$FR+sums$FRIN
sums$GRGRIN<- sums$GR+sums$GRIN
sums$abun<-sums$FF+sums$F+sums$FV+sums$NF

#number of birds of each specialisation in each fragment
sppspec2<-xtabs(No..of.Birds~Forest.specialisation+Fragment.name, data=sppthin)
sppspec2<-as.data.frame(unclass(sppspec2))
sppspec2<-t(sppspec2)
sppspec2
sppthin$plot<-paste(sppthin$Fragment.name,sppthin$Transect)
table(unique(sppthin[,c(8,9,11)])[,1],unique(sppthin[,c(8,9,11)])[,2])

HShann<-diversity(sppagg)
sums<-merge(sums, as.data.frame(HShann), by.x=1, by.y=0)

str(total)
mins<-as.data.frame(aggregate(total[c(5,21:24)], by=list(total$Fragment.name), FUN=min, na.rm=T))
mins$FFprop<-by(total$FF/total$totalbirds, total$Fragment.name, FUN=min)
mins$INGRprop<-by(total$INGR/total$totalbirds, total$Fragment.name, FUN=min)
mins$INGLLprop<-by(total$INGLL/total$totalbirds, total$Fragment.name, FUN=min)
mins$FRprop<-by(total$FR/total$totalbirds, total$Fragment.name, FUN=min)
maxes<-as.data.frame(aggregate(total[c(5,21:24)], by=list(total$Fragment.name), FUN=max, na.rm=T))
maxes$FFprop<-by(total$FF/total$totalbirds, total$Fragment.name, FUN=max)
maxes$INGRprop<-by(total$INGR/total$totalbirds, total$Fragment.name, FUN=max)
maxes$INGLLprop<-by(total$INGLL/total$totalbirds, total$Fragment.name, FUN=max)
maxes$FRprop<-by(total$FR/total$totalbirds, total$Fragment.name, FUN=max)
#reorder levels of fragmentname according to the values of matrixtype
sums$Group.1<-factor(sums$Group.1, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))#reorder to match total
str(sums)
sums<-sums  [order(sums$Group.1),]

#reorder mins and maxes to match
#reorder levels of fragmentname according to the values of matrixtype
mins$Group.1<-factor(mins$Group.1, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))
mins<-mins  [order(mins$Group.1),]
maxes$Group.1<-factor(maxes$Group.1, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))
maxes<-maxes [order(maxes$Group.1),]

#number of species of each specialisation instead of number of individuals - note that each species can have many rows per transect

transects.guilds.byspp<-as.data.frame(cbind((t(sapply(with(sppthin,by(sppthin,paste(Fragment.name,Transect, sep="."), function(s)table(s$Forest.specialisation[!duplicated(s$Spps)]))),I))),(t(sapply(with(sppthin,by(sppthin,paste(Fragment.name,Transect, sep="."), function(s)table(s$Feeding.guild[!duplicated(s$Spps)]))),I)))))# the table I bit turns the array (output of multiple by) into a useful thing. As data frame is still needed because of the cbind

transects.guilds.byspp$Fragment.name<-substr(rownames(transects.guilds.byspp), 1,(nchar(rownames(transects.guilds.byspp))-2))#trim right two chars to get just fragment name back. 
forests.guilds.byspp<-aggregate(No..of.Birds~Fragment.name+Botanical.name+Forest.specialisation+Feeding.guild, data=sppthin, sum)
forests.guilds.byspp<-cbind(as.data.frame(unclass(xtabs((No..of.Birds>0)~Fragment.name+Forest.specialisation, data=forests.guilds.byspp))),as.data.frame(unclass(xtabs((No..of.Birds>0)~Fragment.name+Feeding.guild, data=forests.guilds.byspp)))) 

transects.guilds.byspp$Fragment.name<-factor(transects.guilds.byspp$Fragment.name, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))
transects.guilds.byspp<-transects.guilds.byspp  [order(transects.guilds.byspp$Fragment.name),]

forests.guilds.byspp$Group.1<-factor(rownames(forests.guilds.byspp), levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))
forests.guilds.byspp<-forests.guilds.byspp [order(forests.guilds.byspp$Group.1),]



mins.byspp<-as.data.frame(aggregate(transects.guilds.byspp[1:13], by=list(transects.guilds.byspp$Fragment.name), FUN=min, na.rm=T))

mins.byspp$FFprop<-by(transects.guilds.byspp$FF/rowSums(transects.guilds.byspp[1:4]), transects.guilds.byspp$Fragment.name, FUN=min)
mins.byspp$INGRprop<-by(transects.guilds.byspp$INGR/rowSums(transects.guilds.byspp[5:13]), transects.guilds.byspp$Fragment.name, FUN=min)#this gives 0.312 as the answer
mins.byspp$INGLLprop<-by(transects.guilds.byspp$INGLL/rowSums(transects.guilds.byspp[5:13]), transects.guilds.byspp$Fragment.name, FUN=min)
mins.byspp$FRprop<-by(transects.guilds.byspp$FR/rowSums(transects.guilds.byspp[5:13]), transects.guilds.byspp$Fragment.name, FUN=min)
maxes.byspp<-as.data.frame(aggregate(transects.guilds.byspp[5:13], by=list(transects.guilds.byspp$Fragment.name), FUN=max, na.rm=T))
maxes.byspp$FFprop<-by(transects.guilds.byspp$FF/rowSums(transects.guilds.byspp[1:4]), transects.guilds.byspp$Fragment.name, FUN=max)
maxes.byspp$INGRprop<-by(transects.guilds.byspp$INGR/rowSums(transects.guilds.byspp[5:13]), transects.guilds.byspp$Fragment.name, FUN=max)
maxes.byspp$INGLLprop<-by(transects.guilds.byspp$INGLL/rowSums(transects.guilds.byspp[5:13]), transects.guilds.byspp$Fragment.name, FUN=max)
maxes.byspp$FRprop<-by(transects.guilds.byspp$FR/rowSums(transects.guilds.byspp[5:13]), transects.guilds.byspp$Fragment.name, FUN=max)


mins.byspp$Group.1<-factor(mins.byspp$Group.1, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))
mins.byspp<-mins.byspp  [order(mins.byspp$Group.1),]
maxes.byspp$Group.1<-factor(maxes.byspp$Group.1, levels=c("Busaju","Tengele",  "Ongo", "Rwesama", "Kasokwa","Kinyara","RP7"))
maxes.byspp<-maxes.byspp [order(maxes.byspp$Group.1),]

#needed for splitbarplots
forests.guilds.byspp$FRFRIN<-forests.guilds.byspp$FR+forests.guilds.byspp$FRIN
forests.guilds.byspp$GRGRIN<-forests.guilds.byspp$GR+forests.guilds.byspp$GRIN

#needed for ordination
guildindex<-unique(sppthin[,c(8,9,11)])
guildindex<-guildindex[order(guildindex$Botanical.name),]
guildindex$Feeding.guild<-as.character(guildindex$Feeding.guild)
guildindex$Feeding.guild[guildindex$Feeding.guild=="FR"]<-"Frugivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="FRIN"]<-"Frugivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="GR"]<-"Granivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="GRIN"]<-"Granivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="IN"]<-"Insectivore (general)"
guildindex$Feeding.guild[guildindex$Feeding.guild=="INGLL"]<-"Gleaning insectivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="INGR"]<-"Ground insectivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="INSA"]<-"Sallying insectivore"
guildindex$Feeding.guild[guildindex$Feeding.guild=="NECIN"]<-"Nectarivore"
guildindex$Feeding.guild<-as.factor(guildindex$Feeding.guild)

#write.table(sppthin, "Budongo fragments birds GBIF", sep="\t")

#the weird turnover thingy
sppfeed_weir<-xtabs(No..of.Birds~paste(Feeding.guild, Botanical.name, sep=".")+paste(Fragment.name,Transect, sep="."), data=sppthin)
sppfeed_weir[,13:15]#7 ingr, 5 in all transects.

