#load libraries
library(RODBC)
library(vegan)

#import data
#setwd("D:\\rdata\\Mabira vegetation survey data\\")
setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\Mabira vegetation survey data\\")



con<-odbcConnectExcel("survey_data_corrected.xls")
sqlTables(con)

thinspp<-sqlQuery(con,"Select * From [survey_data$]")
close(con)
sapply(thinspp, class)
head(thinspp)
summary(thinspp)

##some species in the overall herbs dataset need turning into other species - where a Genus sp identification was used and there's a lot of one particular species in that transect.

thinspp$Species_Name[thinspp$Species_Name=='Celtis sp']<-'Celtis durandii'
thinspp$Species_Name[thinspp$Species_Name=='Albizia sp']<-'Albizia gummifera'
thinspp$Species_Name[thinspp$Species_Name=='Albizia 11']<-'Albizia gummifera'
thinspp$Species_Name[thinspp$Species_Name=='Croton sp']<-'Croton macrophyllus'
thinspp$Species_Name[thinspp$Species_Name=='Psydrax sp.']<-'Psydrax parviflora'
thinspp$Species_Name[thinspp$Species_Name=='Allophyllus sp']<-'Allophyllus dummeri'
thinspp$Species_Name[thinspp$Species_Name=='Entandrophragma sp']<-'Entandrophragma cylindricum'
thinspp$Species_Name<-factor(thinspp$Species_Name)


# Removing fern families and species from the herb data
thinspp<-thinspp[!thinspp$Species_Family%in%c("Thelypteridaceae","Pteridaceae","Aspleniaceae"),]
thinspp<-thinspp[!thinspp$Species_Name%in%c("Trifoliate fern"),]
#Taking out unused levels of species from the factor 'species' 
thinspp$Species_Name<-factor(thinspp$Species_Name)
levels(thinspp$Species_Name)



#some checks - nspp should be shorter than nspp_id, seeing what sites had corymborkis (just as an example), and
length(unique(thinspp$species_id))
length(unique(thinspp$Species_Name))
thinspp[thinspp$Species_Name=="Canarium schweinfurthii",]
unique(thinspp$Species_Name)[sapply(unique(thinspp$Species_Name),function(n) length(unique(thinspp$species_id[thinspp$Species_Name==n])))>1]

#########here's where the bits about loading and correcting the data end
#tree data - turn into a fat table, plus make thin table for GBIF  
trees<-xtabs(abundance_count~Transect_id+Species_Name, data=thinspp,subset=Plot==0, drop=T)
trees<-as.data.frame(unclass(trees))
class(trees)
head(trees)

locations<-read.table("Matrix_Mabira_plants_georefs.txt", sep="\t", header=T)
write.csv2((merge(thinspp[thinspp$Plot==0,c(2,5,7:8)], locations, by=1)),"matrix_trees_forgbif.csv") #remove family=NA by hand


#all the ground veg
thinspp2<-thinspp[thinspp$Plot!=0,]

#Joe wrote me a cool function which makes a data frame of aggregated values for one column for each unique level of two other columns.  ... takes arguments for outDataFunc.
plotAggregate <- function(inData, speciesCol, transCol, outDataCol, outDataFunc, ...)
{
  outFrame <- data.frame()
  for(curSpecies in unique(inData[, speciesCol]))
  {
    for(curTransect in unique(inData[, transCol]))
    {
      rowSelector <- inData[, speciesCol] == curSpecies & inData[, transCol] == curTransect
      outVal <- outDataFunc(inData[rowSelector, outDataCol], ...)
      outFrame <- rbind(outFrame, data.frame(
        Species_Name = curSpecies,
        Transect_id = curTransect,
        outVal = outVal
      ))
    }
  }
  names(outFrame)[3] <- outDataCol
  outFrame <- outFrame[!is.na(outFrame[, 3]),]
  outFrame[outFrame[, 3] > 0,]
}

sumdivbysix <- function(inVal)
{
  sum(inVal) / 6.0
}

groundcountsums<-plotAggregate(thinspp2[!(thinspp2$Species_Name %in% c("Dummy", "Allophyllus 10","Collected 9","purple stem","Rubiaceae","Small leaves 1")),], "Species_Name", "Transect_id", "abundance_count", sum)
write.csv2((merge(groundcountsums,locations, by.x=2, by.y=1)), "matrix_groundcounts_forgbif.csv")
groundabunmeans<-plotAggregate(thinspp2[!(thinspp2$Species_Name %in% c("Dummy", "Allophyllus 10","Collected 9","purple stem","Rubiaceae","Small leaves 1")),], "Species_Name", "Transect_id", "abundance_percent", sumdivbysix)
write.csv2((merge(groundabunmeans,locations, by.x=2, by.y=1)), "matrix_groundpercents_forgbif.csv")
####end of joecode

sapply(thinspp2, class)
table(thinspp2$abundance_count)
table(thinspp2$abundance_percent[thinspp2$abundance_count!=0])

table(thinspp2$abundance_percent)
thinspp2[thinspp2$abundance_percent!=0&thinspp2$abundance_count!=0,,drop=F]

#this line turns abundances into percents. things less than 5 abundance get 1% and things more than 5 get 2.
thinspp2$abundance_percent[thinspp2$abundance_count!=0]<-ifelse(thinspp2$abundance_count[thinspp2$abundance_count!=0]<=5,1,2)
#plots per transect
nplots<-tapply(thinspp2$Plot, thinspp2$Transect_id, function(x)length(unique(x)))  

#list of the species names with their habitats, habits and families
ang_hab<-thinspp[7:10]
ang_hab<-unique(ang_hab)
dim(ang_hab)

# creatin of a 'fat' (species x samples) table for herbs, shrubs and seedlings
herbs<-xtabs(abundance_percent~Transect_id+Species_Name, data=thinspp2)
herbs<-as.data.frame(unclass(herbs))/nplots
herbs<-herbs[order(rownames(herbs)),]
head(herbs)
herbs$Dummy<-NULL
dim(herbs)


herbsP<-xtabs(abundance_percent~paste(Transect_id,Plot,sep="_")+Species_Name, data=thinspp2)
herbsP<-as.data.frame(unclass(herbsP))
head(herbsP)
herbsP$Dummy<-NULL

tapply(thinspp2$Plot, thinspp2$Transect_id, function(x)length(unique(x)))  #plots per transect

#add empty transects to trees
x1<-setdiff(rownames(herbs), rownames(trees))
x2<-matrix(0, nrow=length(x1), ncol=ncol(trees))
rownames(x2)<-x1
colnames(x2)<-colnames(trees)
trees<-rbind(trees,x2)
trees<-trees[order(rownames(trees)),]
rm(x1, x2)

setdiff(rownames(trees),rownames(herbs))

####################
#fern data - new way
con3<-odbcConnectExcel("ferntest.xls")
sqlTables(con3)
ferns_thin<-sqlQuery(con3, "Select * From [Species$]")
close(con3)
 
names(ferns_thin)
ferns_thin<-ferns_thin[,c(4,16,17)]
str(ferns_thin)
ferns_thin<-ferns_thin[order(ferns_thin$Transect),]
str(ferns_thin)

write.csv2((merge(ferns_thin[ferns_thin$binomial!="DUMMY",], locations, by=1)),"matrix_ferns_forgbif.csv")

spp<-sort(unique(ferns_thin$binomial))
names(spp)<-spp   

ferns<-sapply(spp, function(sp) 
    sapply(unique(ferns_thin$Transect), function(n) {  print(paste(sp,n))
        x<-ferns_thin$Abundance[ferns_thin$binomial==sp&ferns_thin$Transect==n]
        if(length(x)>0)median(x)
        else 0
    }))
colnames(ferns)<-spp
ferns<-as.data.frame(ferns) 
ferns$DUMMY<-NULL
ferns<-ferns[,order(names(ferns))]
ferns[c(8,9,12,13,14,30,31,32,63,64,65),]<-NA
ferns<-ferns[order(rownames(ferns)),]


#fern data - old way
#fern data including empty transects
#ferns<-read.table("fernsppswithempties.txt", sep="\t", header=T, row.names=1)
#head(ferns)
#ferns<-ferns[order(rownames(ferns)),]
#fern abundance (min val of range given by jenny) for abundance figures
fernabun<-read.table("foresttypeabundances.txt" , sep="\t", header=T, row.names=1)

#fern habitats
fern_hab<-read.table("Fernhabitat.txt", sep="\t", header=T, row.names=1)

#############################


#environmental data
env<-read.table("Envi_alltransects.txt", sep="\t", header=T, row.names=1)
env<-env[order(rownames(env)),]
str(env)


#seed bank data - note that I'm now using the xls version so this needs to be saved/created
ls()
con2<-odbcConnectExcel("Seedbank Datasheet_corrected.xls2.xls")
sqlTables(con2)
thinseeds<-sqlQuery(con2, "Select * From [rawdataback$]")
head(thinseeds)
names(thinseeds)<-c("Compartment","Transect","Species","Family","Group","top1cm3","base4cm3","Total","Habitat")
# thinseeds, seed.all and seed.layers are created here but contaminants are removed in seedsnspores.r 
seed.all<-xtabs(Total~Transect+Species, data=thinseeds)
seed.all<-as.data.frame(unclass(seed.all))
seed.all<-seed.all[order(rownames(seed.all)),]
head(seed.all)
#seed.layers<-xtabs(Transect~top1cm3+base4cm3, data=thinseeds) #gets remade after removal of contaminants.

thinspores<-sqlQuery(con2, "Select * From [Fern data$]")
thinspores<-thinspores[!is.na(thinspores$Transect),]
head(thinspores)
names(thinspores)<-c("<Compartment","Transect","Species","Family","Group","top1cm3","base4cm3","Total")
#the following 6 lines get rewritten in the splitfile version
spore.all<-xtabs(Total~Transect+Species, data=thinspores)
spore.all<-as.data.frame(unclass(spore.all))
spore.all$DUMMY<-NULL
head(spore.all)
spore.all<-spore.all[order(rownames(spore.all)),]
close(con2)



stopifnot(identical(rownames(env),sort(rownames(env))),
  identical(rownames(seed.all),sort(rownames(seed.all))),
  identical(rownames(spore.all),sort(rownames(spore.all))),
  identical(rownames(herbs),sort(rownames(herbs))),
  identical(rownames(ferns),sort(rownames(ferns))),
  identical(rownames(trees),sort(rownames(trees)))
  )

#########################################
bothsetdiff<-function(x,y)list(unique.to.x=setdiff(x,y),unique.to.y=setdiff(y,x), in.common=intersect(x,y))
bothsetdiff(rownames(ferns),rownames(trees))

bothsetdiff(rownames(trees),rownames(spore.all))
bothsetdiff(rownames(trees),rownames(seed.all))
bothsetdiff(rownames(ferns),rownames(seed.all))

bothsetdiff(rownames(ferns),rownames(env))
bothsetdiff(rownames(trees),rownames(env))

bothsetdiff(rownames(seed.all),rownames(env))
bothsetdiff(rownames(seed.all),rownames(ferns))

bothsetdiff(rownames(spore.all),rownames(ferns))

dat<-data.frame(spores=1:74%in%rownames(spore.all),seeds=1:74%in%rownames(seed.all),ferns=1:74%in%rownames(ferns),env=1:74%in%rownames(env) ,herbs=1:74%in%rownames(herbs), trees=1:74%in%rownames(trees), row.names=1:74)
write.table(dat, "data.availability.txt", quote=F)

######################################################
#fill NA rows
fill.na<-function(x){
  x1<-setdiff(1:74, rownames(x))
  x2<-matrix(NA, nrow=length(x1), ncol=ncol(x))
  rownames(x2)<-x1
  colnames(x2)<-colnames(x)
  x<-rbind(x,x2)
  x<-x[order(as.numeric(rownames(x))),]
  x
}

env<-fill.na(env)
ferns<-fill.na(ferns)
herbs<-fill.na(herbs)
trees<-fill.na(trees)
seed.all<-fill.na(seed.all)
spore.all<-fill.na(spore.all)
fernabun<-fill.na(fernabun)

#Make the missing value for Na, then Tcat. In the dataset it's centimols per kg, and this converts that to milliequivalents according to Richard
env$Na<-as.numeric(as.character(env$Na))
env$Na[74]<-mean(env$Na, na.rm=T)
env$TCat<-(env$K+(env$Mg*2)+(env$Ca*2)+env$Na)*10
summary(env$Ca*2*10/env$TCat)
# log transform LLCover
env$LLCover<-log(env$LLCover)


# fill in env where we know the forest age but nothing else:30, 31, 32, 
# nb all false rows so didn't bother: 8, 9, 12, 13, 14, 63, 64, 65
env$Forest[30:32]<-"os"

#make the newage column where 189 is changed from OG to OS
env$newage<-env$Forest
env$newage[env$Compartment==189]<-"os"




env

## then, the dataset is split accorrding to habit, then according to habitat. Note that this is the stage where unidentified species get dropped as unidentified species take NA in the habit list
 
herblist<-ang_hab[ang_hab$Habit=="Nonwoody",]
tail(herblist)
dim(herblist)
trueherbs<-herbs[,which(names(herbs) %in% herblist$Species_Name)]
head(trueherbs)
dim(trueherbs)
trueherbs

treelist<-ang_hab[ang_hab$Habit=="Tree",]
tail(treelist)
dim(treelist)
herbtrees<-herbs[,which(names(herbs) %in% treelist$Species_Name)]
herbtrees<-herbtrees[,colSums(herbtrees,na.rm=T)!=0]# to take out trees which are only present in zero-plots ie are mature trees
head(herbtrees)
dim(herbtrees)


trees<-trees[,which(names(trees) %in% treelist$Species_Name)]
head(trees)
dim(trees)


climberlist<-ang_hab[ang_hab$Habit=="Climber",]
head(climberlist)
dim(climberlist)
herbclimbers<-herbs[,which(names(herbs) %in% climberlist$Species_Name)]
head(herbclimbers)
dim(herbclimbers)

shrublist<-ang_hab[ang_hab$Habit=="Shrub",]
tail(shrublist)
dim(shrublist)
herbshrubs<-herbs[,which(names(herbs) %in% shrublist$Species_Name)]
head(herbshrubs)
dim(herbshrubs)

subcanopywoody<-merge(herbtrees, herbshrubs, by.x=0, by.y=0, sort=F)
head(subcanopywoody)
subcanopywoody$Row.names<-as.numeric(subcanopywoody$Row.names)
subcanopywoody <- subcanopywoody[order(subcanopywoody$Row.names),] 
subcanopywoody$Row.names<-NULL
str(subcanopywoody)


#this isn't working at the moment, it's three species long  because Josephine has entered data for unidentified stuff
idfail_list<-ang_hab[is.na(ang_hab$Habit),]
tail(idfail_list)
idfail<-herbs[,which(names(herbs) %in% idfail_list$Species_Name)]

#report the number of occurrences of identified things and of unidentified things, although this needs correcting same as directly above
sum(idfail>0, na.rm=T)  #8 8/(825+8)= 0.0096 - 1%!
(sum(herbs>0, na.rm=T))+(sum(trees>0, na.rm=T))   #825



#subsets by habitat - currently failing...
#FFset<-herbs[,names(herbs) %in% (ang_hab$Species_Name[(ang_hab$Habitat=="FF"),])]
#EVEN THOUGH THIS WORKS...
levels(ang_hab$Habitat)
FFlist<-ang_hab[ang_hab$Habitat=="FF",]
Flist<-ang_hab[ang_hab$Habitat=="F",]
Glist<-ang_hab[ang_hab$Habitat=="G",]
FFherbsall<-herbs[,which(names(herbs) %in% FFlist$Species_Name)]
Fherbsall<-herbs[,which(names(herbs) %in% Flist$Species_Name)]
Gherbsall<-herbs[,which(names(herbs) %in% Glist$Species_Name)]
FFtrees<-trees[,which(names(trees) %in% FFlist$Species_Name)]
Ftrees<-trees[,which(names(trees) %in% Flist$Species_Name)]
Gtrees<-trees[,which(names(trees) %in% Glist$Species_Name)]


write.csv2(ferns[!is.na(rowSums(ferns)),],file="matrix__Mabira_ferns_forgbif.csv")

#for revisions december 2015 "25 m2 is small – what was the density of samplings?"


