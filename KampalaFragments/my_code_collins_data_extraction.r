library(vegan)
library(ODB)


setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\KampalaFragments\\")
#import data
con<-odb.open("FS1database.odb")
traits<-odb.read(con,'SELECT*from "Taxa"')
odb.close(con)
#thintreetable<-odb.read(con, 'SELECT "Forests"."ForestName", "plot"."plotid", "Taxa"."SpeciesCode", "Taxa"."Genus", "Taxa"."Species", "Taxa"."Author", "tree"."dbh", "tree"."stump" FROM "plot" AS "plot", "Forests" AS "Forests", "tree" AS "tree", "Taxa" AS "Taxa" WHERE "plot"."forestname" = "Forests"."ForestName" AND "tree"."plotkey" = "plot"."key" AND "tree"."speciescode" = "Taxa"."SpeciesCode" AND "tree"."dbh" >= 3.7 AND "tree"."stump" = 0')

#odb.close(con, write=FALSE)
#write.table(thintreetable, "thintreetable2.txt", quote=F)
#str(thintreetable)
#trees<-table(thintreetable$Speciescode,thintreetable$ForestName)
#trees
#treesreally<-xtabs(rep(1,length(thintreetable$speciescode))~thintreetable$ForestName+thintreetable$speciescode)
#trees<-as.data.frame(unclass(trees))
#class(trees)
#head(trees)


fattreetable<- read.table("fattreetable.txt", header=TRUE, sep="\t")
str(fattreetable)

#how many trees were there at each site?
rowSums(fattreetable[,2:193]>0)
#print list of sites
fattreetable$X
#step1: plot a DCA to look at the length of axis 1 to decide between unimodal models (CA, DCA - if axis 1 length is >2) or monotonic models (PCA) - it was 3.84. Then if unimodal look at ca plot. If horrible tick or horseshoe shape, use DCA
 
treeca<-cca(fattreetable[,2:193])
treedca<-decorana(fattreetable[,2:193]) 
plot(treedca)
savePlot("treedca.emf")
#axis scores for sites - the bigger the difference in scores, the more different the composition.
scores(treedca)

#load a list of the trees which have animal-dispersed seeds/fruits
fruitreelist<- read.table("Fruittreelist.csv", header=TRUE, sep=",")    
#check it loaded ok by loading the first 5 lines
head(fruitreelist)
#select from the main data table those species which are in the list of animal-dispersed species
fruittrees<-fattreetable[,which(names(fattreetable) %in% fruitreelist$SpeciesCode)]
#check the data structure, the first five rows, and the dimensions (dim)
str(fruittrees)
head(fruittrees)
dim(fruittrees)
#the number of species that are fruit trees in each forest/year
rowSums(fruittrees>0)
# the proportion of tree species which are fruit species for each forest/year 
(rowSums(fruittrees>0))/(rowSums(fattreetable[,2:193]>0))
#the proportion of trees (by number) which are fruit species - the biggest change is at Kituza where it went from 74% of trees to 47% of trees. Many other forests acually showed a slight rise in the proportion of trees that are fruit trees
(rowSums(fruittrees))/(rowSums(fattreetable[,2:193]))                                        