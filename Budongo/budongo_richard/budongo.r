#Queries & changes
#Rufous crrowned negrofinch - what is this!! and what is its habitat
#greater honeyguide - two habitat preferences


library(vegan)
library(RODBC)
library(gdata)

#import data

redlist<-read.csv("o:/data/uganda/uganda aves redlist.csv")

con<-odbcConnectExcel("bird survey data.xls")

sqlTables(con)
bird0<-   sqlQuery(con, "Select * from [sheet1$]")
names(bird0)
bird0$F12<-NULL
names(bird0)<-trim(names(bird0))
bird0<-bird0[!is.na(bird0$Round),]
sitetype<-factor(toupper(trim(sub("[[:digit:]]", "",levels(bird0$Site)))))
bird0$Species<-tolower(bird0$Species)
head(bird0)


#"blue backed kingfisher"breasted
#  "crocking cisticola"croaking"
   "white throated flycatcher"???semi collared
   "black kite" LC
        "rufous crrowned negrofinch" #??
        
        "cliff chat" Mocking cliff chat
        "velvet mantled drongo"     NOT assessed
        "wood house's ant pecker"     NOT assessed

bird0$Species[bird0$Species=="rufous napeed lark"]<-"rufous naped lark"
bird0$Species[bird0$Species=="grey oliveback"]<-"grey headed oliveback" #?
bird0$Species[bird0$Species=="rufous side broadbill"]<-"rufous sided broadbill"
bird0$Species[bird0$Species=="snow headed robin chat"]<-"snowy headed robin chat"
bird0$Species[bird0$Species=="nothern puffback"]<-"northern puffback"
bird0$Species[bird0$Species=="yellow bill"]<-"yellowbill"
bird0$Species[bird0$Species=="littlle greenbul"]<-"little greenbul"
bird0$Species[bird0$Species=="red cheeked cordon blue"]<-"red cheeked cordon bleu"
bird0$Species[bird0$Species=="black headed gonelek"]<-"black headed gonolek"
bird0$Species[bird0$Species=="zenkers honeyguide"]<-"zenker's honeyguide"
bird0$Species[bird0$Species=="ross' turaco"]<-"ross's turaco"
bird0$Species[bird0$Species=="grey parrots"]<-"grey parrot"
bird0$Species[bird0$Species=="crested guinefowls"]<-"crested guineafowl"
bird0$Species[bird0$Species=="lizzard buzzard"]<-"lizard buzzard" 
bird0$Species[bird0$Species=="veillot's black weaver"]<-"vieillot's black weaver"
bird0$Species[bird0$Species=="grey backed cameroptera"]<-"green backed camaroptera"
bird0$Species[bird0$Species=="african citril"]<-"western citril"
bird0$Species[bird0$Species=="african mousteched warbler"]<-"african moustached warbler"
bird0$Species[bird0$Species=="yellow white eye"]<-"african yellow white-eye"
bird0$Species[bird0$Species=="african open billed stork"]<-"openbill stork"
bird0$Species<-gsub("love-bird","lovebird",bird0$Species)
bird0$Species<-gsub("wood-pecker","woodpecker",bird0$Species)
bird0$Species<-gsub("cameroptera","camaroptera",bird0$Species)

bird0$Species<-gsub("-"," ",bird0$Species)
redlist$Common.names..Eng<-gsub("-"," ",redlist$Common.names..Eng)

which(rowSums(table(bird0$Species,bird0$Forest)>0)>1)
  bird0$Forest[bird0$Species=="greater honeyguide"]

#redlist
table(redlist$Red.List.status)
bird0$redlist<-NA
for(k in unique(redlist$Red.List.status)){  print(k)
  bird0$redlist[bird0$Species%in%tolower( trim(unlist(strsplit(as.character(redlist$Common.names..Eng[redlist$Red.List.status==k]),","))))]<-k
}                                           
rm(k)
table(bird0$redlist, useNA="always")
table(bird0$redlist, useNA="always")/length(bird0$redlist)

table(bird0$Species[is.na(bird0$redlist)])


#map
library(sp)
library(maptools)
plot(bird0[,c("X","Y")])
pos<-unique(bird0[,c(1:3)])
coordinates(pos)<-~X+Y
proj4string(pos)=CRS("+proj=utm +zone=36")
pos<-spTransform(pos, CRS("+proj=longlat +datum=WGS84"))

source("o:/data/uganda/map.r")
#transect<-data.frame(latitude=c(.55,.54, .53),longitude=c(33.0,33.1, 33.05))#you should already have this

bb <- qbbox(c(0.571726,0.569323,0.362993),c(33.175907,32.853184,33.022785));
bb <- qbbox(lat = coordinates(pos)[,2], lon = coordinates(pos)[,1]);

MyMap <- GetMap.bbox(bb$lonR, bb$latR,destfile = "MyTile3.png", maptype = "satellite")

gmap(gm=MyMap, xy=forests2)
PlotOnStaticMap(MyMap, lat = coordinates(pos)[,2], lon = coordinates(pos)[,1],cex=1.5,pch=21,col="white",bg=cols[factor(toupper(trim(sub("[[:digit:]]", "",as.character(pos$Site)))))],add=F);


#####species accumulation curves
acc<-lapply(levels(bird0$Site), function(S){
  specaccum(xtabs(rep(1,nrow(bird0))~paste(bird0$Round,bird0$Point, sep="_")+Species, data=bird0, subset=bird0$Site==S), method="collector")$richness
})

cols<-c("darkgreen","lightblue","blue","green","red")
plot(NA,NA,ylim=c(0,max(sapply(acc, function(a)max(a)))),xlim=c(0,20), xlab="Sampling points", ylab="Number of Species")
for(i in 1:length(acc)){
  lines(acc[[i]], col=cols[sitetype[i]])
}
legend("topleft", legend=levels(sitetype),lty=1, col=cols)

x11()
boxplot( sapply(acc,function(a)a[10])~sitetype, col=cols)


#ordination
bird<-xtabs(rep(1,nrow(bird0))~Site+Species, data=bird0)
bird<-bird>0

decorana(bird[,colSums(bird)>1])

mod<-cca(bird[,colSums(bird)>1])
screeplot(mod, bstick=T)
plot(mod, type="n")
points(mod, disp="sites", col=cols[sitetype])
legend("topleft", legend=levels(sitetype), col=cols, pch=1)

plot(anosim(dat=bird>0, grouping=sitetype, permutations = 999, distance = "bray"))

(dep<-table(bird0$Site, bird0$Forest))
plot(dep, las=4, col=1:7)

(threat<-table(bird0$Site, bird0$redlist))
plot(threat, las=4, col=1:7)

bird0[!is.na(bird0$redlist)&bird0$redlist!="LC",]
